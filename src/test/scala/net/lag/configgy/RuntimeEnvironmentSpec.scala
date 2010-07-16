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

package net.lag.configgy

import net.lag.TestHelper
import _root_.java.io._
import _root_.net.lag.logging.{FileHandler, Logger}
import _root_.org.specs._


class RuntimeEnvironmentSpec extends SpecificationWithJUnit with TestHelper {

  private def writeConfigFile(filename: String, data: String) = {
    val f = new FileOutputStream(folderName + "/" + filename)
    f.write(data.getBytes)
    f.close
  }


  "RuntimeEnvironment" should {
    "allow -D options to override the config file" in {
      withTempFolder {
        val data1 =
          "name=\"Nibbler\"\n" +
          "\n" +
          "<nested>\n" +
          "    level=\"WARNING\"\n" +
          "</nested>\n"
        writeConfigFile("test.conf", data1)

        val runtime = new RuntimeEnvironment(classOf[Config])
        runtime.load(Array("-f", folderName + "/test.conf", "-Dname=Bender", "-D", "nested.extra=tasty"))

        Configgy.config("name") mustEqual "Bender"
        Configgy.config("nested.level") mustEqual "WARNING"
        Configgy.config("nested.extra") mustEqual "tasty"
      }
    }
  }
}
