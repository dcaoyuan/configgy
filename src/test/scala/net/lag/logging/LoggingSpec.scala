/*
 * Copyright 2009 Robey Pointer <robeypointer@gmail.com>
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may
 * not use this file except in compliance with the License. You may obtain
 * a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package net.lag.logging

import _root_.java.io._
import _root_.java.net.{DatagramPacket, DatagramSocket, InetSocketAddress}
import _root_.java.util.{Calendar, Date, TimeZone, logging => javalog}
import _root_.org.specs._
import _root_.net.lag.TestHelper
import _root_.net.lag.configgy.Config
import _root_.net.lag.extensions._


object Crazy {
  def cycle(n: Int): Unit = {
    if (n == 0) {
      throw new Exception("Aie!")
    } else {
      cycle(n - 1)
      Logger.get("").trace("nothing")
    }
  }

  def cycle2(n: Int): Unit = {
    try {
      cycle(n)
    } catch {
      case t: Throwable => throw new Exception("grrrr", t)
    }
  }
}


class TimeWarpingStringHandler extends StringHandler(new FileFormatter) {
  formatter.timeZone = "GMT"

  override def publish(record: javalog.LogRecord) = {
    record.setMillis(1206769996722L)
    super.publish(record)
  }
}


class TimeWarpingSyslogHandler(useIsoDateFormat: Boolean, server: String) extends SyslogHandler(useIsoDateFormat, server) {
  formatter.timeZone = "GMT"

  override def publish(record: javalog.LogRecord) = {
    record.setMillis(1206769996722L)
    super.publish(record)
  }

  getFormatter.asInstanceOf[SyslogFormatter].hostname = "raccoon.local"
}


class ImmediatelyRollingFileHandler(filename: String, policy: Policy, append: Boolean)
      extends FileHandler(filename, policy, new FileFormatter, append) {
  formatter.timeZone = "GMT"

  override def computeNextRollTime(): Long = System.currentTimeMillis + 100

  override def publish(record: javalog.LogRecord) = {
    record.setMillis(1206769996722L)
    super.publish(record)
  }
}


object LoggingSpec extends SpecificationWithJUnit with TestHelper {

  private var handler: Handler = null

  // turn logged console lines into a list of repeatable strings
  private def eat(in: String): List[String] = {
    for (val line <- in.split("\n").toList) yield {
      line.regexSub("""LoggingSpec.scala:\d+""".r) { m => "LoggingSpec.scala:NNN" }
    }.regexSub("""LoggingSpec\$[\w\$]+""".r) {
      m => "LoggingSpec$$"
    }
  }


  "Logging" should {
    doBefore {
      Logger.clearHandlers
      handler = new TimeWarpingStringHandler
      Logger.get("").addHandler(handler)
    }

    doAfter {
      Logger.clearHandlers
    }

    "provide level name and value maps" in {
      Logger.levels mustEqual Map(Level.ALL.value -> Level.ALL, Level.TRACE.value -> Level.TRACE, 
        Level.DEBUG.value -> Level.DEBUG, Level.INFO.value -> Level.INFO, Level.WARNING.value -> Level.WARNING, 
        Level.ERROR.value -> Level.ERROR, Level.CRITICAL.value -> Level.CRITICAL, Level.FATAL.value -> Level.FATAL, 
        Level.OFF.value -> Level.OFF)
      Logger.levelNames mustEqual Map("ALL" -> Level.ALL, "TRACE" -> Level.TRACE, "DEBUG" -> Level.DEBUG,
        "INFO" -> Level.INFO, "WARNING" -> Level.WARNING, "ERROR" -> Level.ERROR,
        "CRITICAL" -> Level.CRITICAL, "FATAL" -> Level.FATAL, "OFF" -> Level.OFF)
    }

    "perform basic logging" in {
      val log = Logger("")
      log.error("error!")
      eat(handler.toString) mustEqual List("ERR [20080329-05:53:16.722] (root): error!")
      handler.asInstanceOf[StringHandler].clear
      // must not do sprintf encoding with only one parameter.
      log.error("error-%s")
      eat(handler.toString) mustEqual List("ERR [20080329-05:53:16.722] (root): error-%s")
    }

    "do lazy message evaluation" in {
      val log = Logger.get("")
      var callCount = 0
      def getSideEffect = {
        callCount += 1
        "ok"
      }
      // add 2nd handler:
      log.addHandler(new TimeWarpingStringHandler)
      log.ifError("this is " + getSideEffect)
      // should not generate since it's not handled:
      log.ifDebug("this is not " + getSideEffect)

      eat(handler.toString) mustEqual List("ERR [20080329-05:53:16.722] (root): this is ok")
      // verify that the string was generated exactly once, even tho we logged it to 2 handlers:
      callCount mustEqual 1
    }

    // verify that we can ask logs to be written in UTC
    "log in utc when asked to" in {
      val log = Logger.get("")
      log.getHandlers()(0).asInstanceOf[Handler].useUtc = true
      log.error("error!")
      eat(handler.toString) mustEqual List("ERR [20080329-05:53:16.722] (root): error!")
    }

    "figure out package names" in {
      val log1 = Logger(getClass)
      log1.name mustEqual "net.lag.logging.LoggingSpec"
    }

    "log package names" in {
      val log1 = Logger.get("net.lag.logging.Skeletor")
      log1.warning("I am coming for you!")
      val log2 = Logger.get("net.lag.configgy.Skeletor")
      log2.warning("I am also coming for you!")

      eat(handler.toString) mustEqual
        List("WAR [20080329-05:53:16.722] logging: I am coming for you!",
             "WAR [20080329-05:53:16.722] configgy: I am also coming for you!")

      handler.asInstanceOf[StringHandler].clear
      handler.formatter.useFullPackageNames = true
      log1.warning("I am coming for you!")
      log2.warning("I am also coming for you!")
      eat(handler.toString) mustEqual
        List("WAR [20080329-05:53:16.722] net.lag.logging: I am coming for you!",
             "WAR [20080329-05:53:16.722] net.lag.configgy: I am also coming for you!")
    }

    "log level names" in {
      val log1 = Logger.get("net.lag.logging.Skeletor")
      log1.setLevel(Level.DEBUG)
      log1.warning("I am coming for you!")
      log1.debug("Loading supplies...")
      log1.trace("Catfood query.")
      log1.error("Help!")

      eat(handler.toString) mustEqual
        List("WAR [20080329-05:53:16.722] logging: I am coming for you!",
             "DEB [20080329-05:53:16.722] logging: Loading supplies...",
             "ERR [20080329-05:53:16.722] logging: Help!")
    }

    "use a crazy formatter" in {
      val formatter = new GenericFormatter("%2$s <HH:mm> %1$.4s ")
      formatter.timeZone = "GMT"
      handler.setFormatter(formatter)
      val log = Logger.get("net.lag.logging.Skeletor")
      log.error("Help!")
      eat(handler.toString) mustEqual List("logging 05:53 ERRO Help!")
    }

    "truncate lines" in {
      handler.truncateAt = 30
      val log1 = Logger.get("net.lag.whiskey.Train")
      log1.critical("Something terrible happened that may take a very long time to explain because I write crappy log messages.")

      eat(handler.toString) mustEqual
        List("CRI [20080329-05:53:16.722] whiskey: Something terrible happened th...")
    }

    "honor append setting on logfiles" in {
      withTempFolder {
        val f = new OutputStreamWriter(new FileOutputStream(folderName + "/test.log"), "UTF-8")
        f.write("hello!\n")
        f.close

        val rollHandler = new ImmediatelyRollingFileHandler(folderName + "/test.log", Hourly, true)
        val log = Logger.get("net.lag.whiskey.Train")
        val date = new Date()
        log.addHandler(rollHandler)
        log.fatal("first line.")

        val f2 = new BufferedReader(new InputStreamReader(new FileInputStream(folderName +
          "/test.log")))
        f2.readLine mustEqual "hello!"
      }

      withTempFolder {
        val f = new OutputStreamWriter(new FileOutputStream(folderName + "/test.log"), "UTF-8")
        f.write("hello!\n")
        f.close

        val rollHandler = new ImmediatelyRollingFileHandler(folderName + "/test.log", Hourly, false)
        val log = Logger.get("net.lag.whiskey.Train")
        val date = new Date()
        log.addHandler(rollHandler)
        log.fatal("first line.")

        val f2 = new BufferedReader(new InputStreamReader(new FileInputStream(folderName +
          "/test.log")))
        f2.readLine mustEqual "FAT [20080329-05:53:16.722] whiskey: first line."
      }
    }

    "write stack traces" in {
      handler.truncateStackTracesAt = 5
      val log1 = Logger.get("net.lag.whiskey.Train")
      try {
        Crazy.cycle(10)
      } catch {
        case t: Throwable => log1.error(t, "Exception!")
      }

      eat(handler.toString) mustEqual
        List("ERR [20080329-05:53:16.722] whiskey: Exception!",
             "ERR [20080329-05:53:16.722] whiskey: java.lang.Exception: Aie!",
             "ERR [20080329-05:53:16.722] whiskey:     at net.lag.logging.Crazy$.cycle(LoggingSpec.scala:NNN)",
             "ERR [20080329-05:53:16.722] whiskey:     at net.lag.logging.Crazy$.cycle(LoggingSpec.scala:NNN)",
             "ERR [20080329-05:53:16.722] whiskey:     at net.lag.logging.Crazy$.cycle(LoggingSpec.scala:NNN)",
             "ERR [20080329-05:53:16.722] whiskey:     at net.lag.logging.Crazy$.cycle(LoggingSpec.scala:NNN)",
             "ERR [20080329-05:53:16.722] whiskey:     at net.lag.logging.Crazy$.cycle(LoggingSpec.scala:NNN)",
             "ERR [20080329-05:53:16.722] whiskey:     (...more...)")
    }

    "write nested stack traces" in {
      handler.truncateStackTracesAt = 2
      val log1 = Logger.get("net.lag.whiskey.Train")
      try {
        Crazy.cycle2(2)
      } catch {
        case t: Throwable => log1.error(t, "Exception!")
      }

      eat(handler.toString) mustEqual
        List("ERR [20080329-05:53:16.722] whiskey: Exception!",
             "ERR [20080329-05:53:16.722] whiskey: java.lang.Exception: grrrr",
             "ERR [20080329-05:53:16.722] whiskey:     at net.lag.logging.Crazy$.cycle2(LoggingSpec.scala:NNN)",
             "ERR [20080329-05:53:16.722] whiskey:     at net.lag.logging.LoggingSpec$$.apply(LoggingSpec.scala:NNN)",
             "ERR [20080329-05:53:16.722] whiskey:     (...more...)",
             "ERR [20080329-05:53:16.722] whiskey: Caused by java.lang.Exception: Aie!",
             "ERR [20080329-05:53:16.722] whiskey:     at net.lag.logging.Crazy$.cycle(LoggingSpec.scala:NNN)",
             "ERR [20080329-05:53:16.722] whiskey:     at net.lag.logging.Crazy$.cycle(LoggingSpec.scala:NNN)",
             "ERR [20080329-05:53:16.722] whiskey:     (...more...)")
    }


    "roll logs on time" in {
      "hourly" in {
        withTempFolder {
          val rollHandler = new FileHandler(folderName + "/test.log", Hourly, new FileFormatter, true)
          rollHandler.computeNextRollTime(1206769996722L) mustEqual 1206770400000L
          rollHandler.computeNextRollTime(1206770400000L) mustEqual 1206774000000L
          rollHandler.computeNextRollTime(1206774000001L) mustEqual 1206777600000L
        }
      }

      "weekly" in {
        withTempFolder {
          val formatter = new FileFormatter
          formatter.calendar.setTimeZone(TimeZone.getTimeZone("GMT-7:00"))
          val rollHandler = new FileHandler(folderName + "/test.log", Weekly(Calendar.SUNDAY), formatter, true)
          rollHandler.computeNextRollTime(1250354734000L) mustEqual 1250406000000L
          rollHandler.computeNextRollTime(1250404734000L) mustEqual 1250406000000L
          rollHandler.computeNextRollTime(1250406001000L) mustEqual 1251010800000L
          rollHandler.computeNextRollTime(1250486000000L) mustEqual 1251010800000L
          rollHandler.computeNextRollTime(1250496000000L) mustEqual 1251010800000L
        }
      }
    }

    // verify that at the proper time, the log file rolls and resets.
    "roll logs into new files" in {
      withTempFolder {
        val rollHandler = new ImmediatelyRollingFileHandler(folderName + "/test.log", Hourly, true)
        val log = Logger.get("net.lag.whiskey.Train")
        val date = new Date()
        log.addHandler(rollHandler)
        log.fatal("first file.")

        Thread.sleep(150)

        log.fatal("second file.")
        rollHandler.close()

        val movedFilename = folderName + "/test-" + rollHandler.timeSuffix(date) + ".log"
        new BufferedReader(new InputStreamReader(new FileInputStream(movedFilename), "UTF-8")).readLine mustEqual
          "FAT [20080329-05:53:16.722] whiskey: first file."
        new BufferedReader(new InputStreamReader(new FileInputStream(folderName + "/test.log"), "UTF-8")).readLine mustEqual
          "FAT [20080329-05:53:16.722] whiskey: second file."
      }
    }

    "write syslog entries" in {
      // start up new syslog listener
      val serverSocket = new DatagramSocket
      val serverPort = serverSocket.getLocalPort

      var syslog = new TimeWarpingSyslogHandler(true, "localhost:" + serverPort)
      val log = Logger.get("net.lag.whiskey.Train")
      log.addHandler(syslog)
      log.setLevel(Level.DEBUG)

      log.fatal("fatal message!")
      log.error("error message!")
      syslog.serverName = "pingd"
      log.warning("warning message!")
      syslog.clearServerName
      log.debug("and debug!")

      Future.sync
      val p = new DatagramPacket(new Array[Byte](1024), 1024)
      serverSocket.receive(p)
      new String(p.getData, 0, p.getLength) mustEqual "<9>2008-03-29T05:53:16 raccoon.local whiskey: fatal message!"
      serverSocket.receive(p)
      new String(p.getData, 0, p.getLength) mustEqual "<11>2008-03-29T05:53:16 raccoon.local whiskey: error message!"
      serverSocket.receive(p)
      new String(p.getData, 0, p.getLength) mustEqual "<12>2008-03-29T05:53:16 raccoon.local [pingd] whiskey: warning message!"
      serverSocket.receive(p)
      new String(p.getData, 0, p.getLength) mustEqual "<15>2008-03-29T05:53:16 raccoon.local whiskey: and debug!"

      log.removeHandler(syslog)
      syslog = new TimeWarpingSyslogHandler(false, "localhost:" + serverPort)
      log.addHandler(syslog)
      log.info("here's an info message with BSD time.")
      serverSocket.receive(p)
      Future.sync
      new String(p.getData, 0, p.getLength) mustEqual "<14>Mar 29 05:53:16 raccoon.local whiskey: here's an info message with BSD time."
    }


    "configure logging" in {
      withTempFolder {
        val TEST_DATA =
          "node=\"net.lag\"\n" +
          "filename=\"" + folderName + "/test.log\"\n" +
          "level=\"debug\"\n" +
          "truncate=1024\n" +
          "use_full_package_names = true\n" +
          "prefix_format=\"%s <HH:mm> %s\"\n" +
          "append off\n"

        val c = new Config
        c.load(TEST_DATA)
        val log = Logger.configure(c, false, false)

        log.getLevel mustEqual Level.DEBUG
        log.getHandlers.length mustEqual 1
        val handler = log.getHandlers()(0).asInstanceOf[FileHandler]
        handler.filename mustEqual folderName + "/test.log"
        handler.append mustEqual false
        handler.formatter.formatPrefix(javalog.Level.WARNING, "10:55", "hello") mustEqual "WARNING 10:55 hello"
        log.name mustEqual "net.lag"
        handler.truncateAt mustEqual 1024
        handler.formatter.useFullPackageNames mustEqual true
      }

      withTempFolder {
        val TEST_DATA =
          "node=\"net.lag\"\n" +
          "syslog_host=\"example.com:212\"\n" +
          "syslog_server_name=\"elmo\"\n" +
          "syslog_priority=128\n"

        val c = new Config
        c.load(TEST_DATA)
        val log = Logger.configure(c, false, true)

        log.getHandlers.length mustEqual 1
        val h = log.getHandlers()(0).asInstanceOf[SyslogHandler]
        h.dest.asInstanceOf[InetSocketAddress].getHostName mustEqual "example.com"
        h.dest.asInstanceOf[InetSocketAddress].getPort mustEqual 212
        h.serverName mustEqual "elmo"
        h.priority mustEqual 128
      }
    }

    "handle config errors" in {
      // should throw an exception because of the unknown attribute
      val TEST_DATA =
        "filename=\"foobar.log\"\n" +
        "level=\"debug\"\n" +
        "style=\"html\"\n"

      val c = new Config
      c.load(TEST_DATA)
      Logger.configure(c, false, false) must throwA(new LoggingException("Unknown logging config attribute(s): style"))
    }

    "build a scribe RPC call" in {
      val formatter = new FileFormatter
      formatter.timeZone = "GMT"
      val scribe = new ScribeHandler(formatter)
      scribe.category = "test"
      Logger.get("").addHandler(scribe)
      Logger.get("hello").info("This is a message.")
      Logger.get("hello").info("This is another message.")
      scribe.makeBuffer(2).array.hexlify mustEqual "000000b080010001000000034c6f67000000000f0001" +
        "0c000000020b000100000004746573740b000200000036494e46205b32303038303332392d30353a35333a3" +
        "1362e3732325d2068656c6c6f3a20546869732069732061206d6573736167652e0a000b0001000000047465" +
        "73740b00020000003c494e46205b32303038303332392d30353a35333a31362e3732325d2068656c6c6f3a2" +
        "05468697320697320616e6f74686572206d6573736167652e0a0000"
    }

    "throw away log messages if scribe is too busy" in {
      val scribe = new ScribeHandler(new GenericFormatter(""))
      scribe.category = "test"
      scribe.maxMessagesToBuffer = 1
      scribe.bufferTimeMilliseconds = 5000
      Logger.get("").addHandler(scribe)
      Logger.get("hello").info("This is a message.")
      Logger.get("hello").info("This is another message.")
      scribe.queue.toList mustEqual List("This is another message.\n")
    }

    "configure a scribe server" in {
      val TEST_DATA =
        "scribe_server = \"fake:8080\"\n" +
        "scribe_buffer_msec = 333\n" +
        "scribe_backoff_msec = 501\n" +
        "scribe_max_packet_size = 66\n" +
        "scribe_category = \"stats\"\n" +
        "scribe_max_buffer = 102\n"
      val c = new Config
      c.load(TEST_DATA)
      val log = Logger.configure(c, false, true)
      val handler = log.getHandlers()(0).asInstanceOf[ScribeHandler]

      handler.server mustEqual "fake:8080"
      handler.bufferTimeMilliseconds mustEqual 333
      handler.connectBackoffMilliseconds mustEqual 501
      handler.maxMessagesPerTransaction mustEqual 66
      handler.category mustEqual "stats"
      handler.maxMessagesToBuffer mustEqual 102
    }

    "set two handlers on the same logger without resetting the level" in {
      val TEST_DATA =
        "filename=\"foobar.log\"\n" +
        "level=\"debug\"\n" +
        "scribe {\n" +
        "  scribe_server = \"fake:8080\"\n" +
        "  level=\"fatal\"\n" +
        "}\n"
      val c = new Config
      c.load(TEST_DATA)
      val log1 = Logger.configure(c, false, true)
      val log2 = Logger.configure(c.configMap("scribe"), false, false)
      log1.getLevel mustEqual Logger.DEBUG
      log2.getLevel mustEqual Logger.DEBUG
      log1.getHandlers()(0) must haveClass[FileHandler]
      log1.getHandlers()(0).getLevel mustEqual Logger.DEBUG
      log1.getHandlers()(1) must haveClass[ScribeHandler]
      log1.getHandlers()(1).getLevel mustEqual Logger.FATAL
    }
  }
}
