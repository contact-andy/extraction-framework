package org.dbpedia.extraction.server.stats

import java.io.{File,FileOutputStream,OutputStreamWriter}
import java.util.logging.Logger
import org.dbpedia.extraction.transform.Quad

import scala.collection.mutable
import org.dbpedia.extraction.util.StringUtils.prettyMillis
import org.dbpedia.extraction.util.{Language,WikiUtil,IOUtils}
import org.dbpedia.extraction.util.RichFile.wrapFile
import org.dbpedia.extraction.util.TurtleUtils.unescapeTurtle

class MappingStatsBuilder(statsDir : File, language: Language, pretty: Boolean)
extends MappingStatsConfig(statsDir, language)
{
    private val logger = Logger.getLogger(getClass.getName)

    private val resourceUriPrefix = language.resourceUri.append("")
    private val propertyUriPrefix = language.propertyUri.append("")

    def buildStats(redirectsFile: File, articleTemplatesFile: File, templateParametersFile: File, infoboxTestFile: File, infoboxPropertiesFile: File): Unit =
    {
      var templatesMap = new mutable.HashMap[String, TemplateStatsBuilder]()
      val pageToTemplate = mutable.Map[String, Seq[String]]()

      println("Reading redirects from " + redirectsFile)
      val redirects = loadTemplateRedirects(redirectsFile)
      println("Found " + redirects.size + " redirects")
      
      println("Using Template namespace " + templateNamespace + " for language " + language.wikiCode)
      
      println("Counting templates in " + articleTemplatesFile)
      countTemplates(articleTemplatesFile, templatesMap, pageToTemplate, redirects)
      println("Found " + templatesMap.size + " different templates")

      println("Loading property definitions from " + templateParametersFile)
      propertyDefinitions(templateParametersFile, templatesMap, redirects)

      println("Counting properties in " + infoboxTestFile)
      countProperties(infoboxTestFile, templatesMap, redirects)
      

      // Count templates where at least one property occurs in >= 10% of instances.
      // This filters out templates that are mostly empty and therefore less useful for DBpedia.
      val qualifyingTemplateCount = templatesMap.count { 
        case (_, stats) => 
          stats.properties.nonEmpty && (stats.properties.values.max * 10 > stats.templateCount)
      }

      // If no templates meet the criteria, attempt counting again using the infobox-properties file
      if (qualifyingTemplateCount == 0) {
        println("##############################################################################")
        println(s"No qualifying templates found in the initial property count.")
        println(s"Retrying property counting using: $infoboxPropertiesFile")
        println("##############################################################################")
        countPropertiesFromInfoboxProps(infoboxPropertiesFile, templatesMap, redirects,pageToTemplate)
      }
      
      // Retain only the templates for which some properties occur in at least 10% of template
      // instances. Templates that are usually empty are unlikely to be useful for DBpedia.
      templatesMap.retain((k, v) => v.properties.nonEmpty && v.properties.values.max * 10 > v.templateCount)
      val builtTemplatesMap = templatesMap.mapValues(_.build)
        
      val wikiStats = new WikipediaStats(language, redirects.toMap, builtTemplatesMap.toMap)
      
      logger.info("Serializing "+language.wikiCode+" wiki statistics to " + mappingStatsFile)
      val output = new OutputStreamWriter(new FileOutputStream(mappingStatsFile), "UTF-8")
      try wikiStats.write(output) finally output.close()
    }

    private def eachLine(file: File)(process: String => Unit) : Unit = {
      val millis = System.currentTimeMillis
      var count = 0
      IOUtils.readLines(file) { line =>
        if (line != null) {
          process(line)
          count += 1
          if (count % 1000000 == 0) {
              if (pretty) print(count+" lines\r")
              else println(count+" lines")
          }
        }
      }
      println(count+" lines - "+prettyMillis(System.currentTimeMillis - millis))
    }

    private def loadTemplateRedirects(file: File): mutable.Map[String, String] =
    {
      val redirects = new mutable.HashMap[String, String]()
      eachLine(file) {
        line => line.trim match {
          case Quad(quad) if (quad.datatype == null) => {
            val templateName = cleanUri(quad.subject)
            if (templateName.startsWith(templateNamespace)) {
              redirects(templateName) = cleanUri(quad.value)
            }
          }
          case str => if (str.nonEmpty && ! str.startsWith("#")) throw new IllegalArgumentException("line did not match object triple syntax: " + line)
        }
      }
      
      redirects
    }
    
    /**
     * Our wikitext parser is not very precise - some template or parameter names are broken.
     * Only use names that contain none of the following chars: < > { | }
     */
    private def goodName(name: String): Boolean =
    {
      for (c <- name) if (c == '<' || c == '>' || c == '{' || c == '|' || c == '}') return false
      return true
    }

    /**
     * @param fileName name of file generated by InfoboxExtractor, e.g. infobox_properties_en.nt
     */
    private def countTemplates(file: File, resultMap: mutable.Map[String, TemplateStatsBuilder], pageToTemplate: mutable.Map[String, Seq[String]],redirects: mutable.Map[String, String]): Unit =
    {
      // iterate through infobox properties
      eachLine(file) {
        line => line.trim match {
          // predicate must be wikiPageUsesTemplate
          case Quad(quad) if (quad.datatype == null && unescapeTurtle(quad.predicate).contains("wikiPageUsesTemplate")) =>
          {
            var templateName = cleanUri(quad.value)
            val pageTitle = cleanUri(quad.subject) // Page title

            
            if (goodName(templateName))
            {
              // resolve redirect for *object*
              templateName = redirects.getOrElse(templateName, templateName)
  
              // lookup the *object* in the resultMap, create a new TemplateStats object if not found,
              // and increment templateCount
              resultMap.getOrElseUpdate(templateName, new TemplateStatsBuilder).templateCount += 1
              pageToTemplate.put(pageTitle, (pageToTemplate.getOrElse(pageTitle, Seq()) :+ templateName).distinct)

            }
          }
          case str => if (str.nonEmpty && ! str.startsWith("#")) throw new IllegalArgumentException("line did not match object or datatype triple syntax: " + line)
        }
      }
    }
    
    private def propertyDefinitions(file: File, resultMap: mutable.Map[String, TemplateStatsBuilder], redirects: mutable.Map[String, String]): Unit =
    {
      // iterate through template parameters
      eachLine(file) {
        line => line.trim match {
          case Quad(quad) =>
          {
            var templateName = cleanUri(quad.subject)
            val propertyName = cleanValue(quad.value)
            
            if (goodName(propertyName))
            {
              // resolve redirect for *subject*
              templateName = redirects.getOrElse(templateName, templateName)
                  
              // lookup the *subject* in the resultMap
              // skip the templates that are not used in any page
              for (stats <- resultMap.get(templateName))
              {
                // add object to properties map with count 0
                stats.properties.put(propertyName, 0)
              }
              
            }
            
          }
          case str => if (str.nonEmpty && ! str.startsWith("#")) throw new IllegalArgumentException("line did not match datatype triple syntax: " + line)
        }
      }
    }

    private def countProperties(file: File, resultMap: mutable.Map[String, TemplateStatsBuilder], redirects: mutable.Map[String, String]) : Unit =
    {
        // iterate through infobox test
        eachLine(file) {
            line => line.trim match {
                case Quad(quad) if (quad.datatype != null) => {
                    var templateName = cleanUri(quad.predicate)
                    val propertyName = cleanValue(quad.value)
                    
                    // resolve redirect for template
                    templateName = redirects.getOrElse(templateName, templateName)

                    // lookup the template in the resultMap
                    // skip the templates that are not found (they don't occur in Wikipedia)
                    for(stats <- resultMap.get(templateName)) {
                        // lookup property in the properties map
                        // skip the properties that are not found with any count (they don't occur in the template definition)
                        if (stats.properties.contains(propertyName)) {
                            // increment count in properties map
                            stats.properties.put(propertyName, stats.properties(propertyName) + 1)
                        }
                    }
                }
                case str => if (str.nonEmpty && ! str.startsWith("#")) throw new IllegalArgumentException("line did not match datatype triple syntax: " + line)
            }
        }
    }
    // Normalization step before comparing
    private def normalizePropertyName(name: String): String = {
      name.replace("_", "").trim
    }
    // Modified countProperties to use infobox-properties.ttl
    private def countPropertiesFromInfoboxProps(
        file: File,
        resultMap: mutable.Map[String, TemplateStatsBuilder],
        redirects: mutable.Map[String, String],
        pageToTemplate: mutable.Map[String, Seq[String]] // from buildPageTemplateMap
    ): Unit = {

      eachLine(file) { line =>
        line.trim match {
          case Quad(quad) =>
            val pageTitle = cleanUri(quad.subject)       // Page title
            val propertyName = cleanUri(quad.predicate)  // Property name

            // Lookup template for this page
            // for (templateNameRaw <- pageToTemplate.get(pageTitle)) {
            for {
                templateList <- pageToTemplate.get(pageTitle) // Option[Seq[String]]
                templateNameRaw <- templateList               // iterate each template in the list
            } {
              var templateName = redirects.getOrElse(templateNameRaw, templateNameRaw)
              // println("templateName:"+templateName)
              // Only update if template is already tracked
              for (stats <- resultMap.get(templateName)) {
                // println("propertyName:"+propertyName)
                if (stats.properties.contains(propertyName)) {
                  stats.properties.put(
                    propertyName,
                    stats.properties(propertyName) + 1
                  )
                }
                else {
                  val normalizedTarget = normalizePropertyName(propertyName)

                  stats.properties.keys.find { k =>
                    normalizePropertyName(k) == normalizedTarget
                  } match {
                    case Some(matchingKey) =>
                      stats.properties.put(
                        matchingKey,
                        stats.properties(matchingKey) + 1
                      )
                    case _ => // Do nothing if no match
                  }
                }

              }
            }

          case str =>
            if (str.nonEmpty && !str.startsWith("#"))
              throw new IllegalArgumentException(
                "line did not match triple syntax: " + line
              )
        }
      }
    }


    // private def stripUri(uri: String): String = {
    //     if (! uri.startsWith(resourceUriPrefix)) throw new Exception(uri)
    //     WikiUtil.wikiDecode(uri.substring(resourceUriPrefix.length))
    // }
    private def stripUri(uri: String): String = {
        if (uri.startsWith(resourceUriPrefix)) {
            WikiUtil.wikiDecode(uri.substring(resourceUriPrefix.length))
        } else if (uri.startsWith(propertyUriPrefix)) {
            WikiUtil.wikiDecode(uri.substring(propertyUriPrefix.length))
        } else if (uri.startsWith("http://dbpedia.org/property/")) {
            WikiUtil.wikiDecode(uri.substring("http://dbpedia.org/property/".length))
        } else {
            throw new Exception(s"Unrecognized URI: $uri")
        }
    }
    
    private def cleanUri(uri: String) : String = cleanName(stripUri(unescapeTurtle(uri)))
    
    private def cleanValue(value: String) : String = cleanName(unescapeTurtle(value))
    
    // values may contain line breaks, which mess up our file format, so let's remove them.
    private def cleanName(name: String) : String = name.replaceAll("\r|\n", "")

}
