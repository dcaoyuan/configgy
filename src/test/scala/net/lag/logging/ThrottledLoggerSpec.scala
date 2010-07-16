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

import net.lag.TestHelper
import _root_.org.specs._

object ThrottledLoggerSpec extends Specification with TestHelper {
  private var handler: Handler = null

  "ThrottledLogger" should {
    doBefore {
      Logger.clearHandlers
      handler = new StringHandler(new GenericFormatter(""))
      Logger.get("").addHandler(handler)
    }

    doAfter {
      Logger.clearHandlers
    }

    "throttle keyed log messages" in {
      val log = Logger()
      val throttledLog = new ThrottledLogger[String](log, 1000, 3)
      throttledLog.error("apple", "help!")
      throttledLog.error("apple", "help 2!")
      throttledLog.error("orange", "orange!")
      throttledLog.error("orange", "orange!")
      throttledLog.error("apple", "help 3!")
      throttledLog.error("apple", "help 4!")
      throttledLog.error("apple", "help 5!")
      throttledLog.reset()
      throttledLog.error("apple", "done.")

      handler.toString.split("\n").toList mustEqual List("help!", "help 2!", "orange!", "orange!", "help 3!", "(swallowed 2 repeating messages)", "done.")
    }
  }
}
