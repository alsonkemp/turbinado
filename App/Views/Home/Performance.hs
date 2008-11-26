page :: View XML
page =      <div>
              <h1>Performance</h1>
              <p>
                Turbinado is written in Haskell, so has the speed of an application framework written in a compiled language. In order to test the relative performance, Turbinado is compared to Apache and Ruby On Rails.All tests were conducted with the following command against a 1.5GHz Pentium-M ThinkPad T40 running Debian Unstable.
              </p>

              <h1>Dynamic Content</h1>
              <p>The following command was used for the static test:</p>
              <pre>sudo ab -n 1000 -c 10 http://localhost/welcome/hello (or /Home/Hello)</pre>
              <h2>Turbinado - (Simple) Dynamic Content - LogLevel ERROR</h2>
              <pre>
                Concurrency Level:      10
                Time taken for tests:   1.316121 seconds
                Complete requests:      1000
                Failed requests:        0
                Write errors:           0
                Total transferred:      142000 bytes
                HTML transferred:       11000 bytes
                Requests per second:    759.81 [#/sec] (mean)
                Time per request:       13.161 [ms] (mean)
                Time per request:       1.316 [ms] (mean, across all concurrent requests)
                Transfer rate:          104.85 [Kbytes/sec] received
              </pre>
              <h2>Ruby On Rails - Dynamic Content - Mongrel</h2>
              <p>Corrected as per GitHub message from <a href="http://github.com/lifo">lifo</a>.</p>
              <pre>
                Concurrency Level:      10
                Time taken for tests:   5.956132 seconds
                Complete requests:      1000
                Failed requests:        0
                Write errors:           0
                Total transferred:      470000 bytes
                HTML transferred:       12000 bytes
                Requests per second:    167.89 [#/sec] (mean)
                Time per request:       59.561 [ms] (mean)
                Time per request:       5.956 [ms] (mean, across all concurrent requests)
                Transfer rate:          76.90 [Kbytes/sec] received
              </pre>
             
              <h1>Static Content</h1>
              <p>The following command was used for the static test:</p>
              <pre>sudo ab -n 10000 -c 10 http://localhost/images/1x1.gif</pre>
              <h2>Turbinado - Static Content - LogLevel ERROR</h2>
              <pre>
                Concurrency Level:      10
                Time taken for tests:   6.78643 seconds
                Complete requests:      10000
                Failed requests:        0
                Write errors:           0
                Total transferred:      1600000 bytes
                HTML transferred:       350000 bytes
                Requests per second:    1645.10 [#/sec] (mean)
                Time per request:       6.079 [ms] (mean)
                Time per request:       0.608 [ms] (mean, across all concurrent requests)
                Transfer rate:          256.97 [Kbytes/sec] received
                Concurrency Level:      10
              </pre>

              <h2>Apache - Static Content</h2>
              <pre>
                Concurrency Level:      10
                Time taken for tests:   2.756945 seconds
                Complete requests:      10000
                Failed requests:        0
                Write errors:           0
                Total transferred:      2870000 bytes
                HTML transferred:       350000 bytes
                Requests per second:    3627.20 [#/sec] (mean)
                Time per request:       2.757 [ms] (mean)
                Time per request:       0.276 [ms] (mean, across all concurrent requests)
                Transfer rate:          1016.34 [Kbytes/sec] received
              </pre>
              <h2>Rails/Mongrel - Static Content</h2>
              <pre>
                Concurrency Level:      10
                Time taken for tests:   21.980972 seconds
                Complete requests:      10000
                Failed requests:        0
                Write errors:           0
                Total transferred:      2290000 bytes
                HTML transferred:       350000 bytes
                Requests per second:    454.94 [#/sec] (mean)
                Time per request:       21.981 [ms] (mean)
                Time per request:       2.198 [ms] (mean, across all concurrent requests)
     `          Transfer rate:          101.72 [Kbytes/sec] received
              </pre>
            </div>
