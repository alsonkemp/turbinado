page =      <div>
              <h1>Installation == Pain, Pain == Love</h1>
              <p>
                Given the relative immaturity of Haskell's package installation tools, installation of Turbinado is fairly challenging.  With <% anchorTag "http://hackage.haskell.org/trac/hackage/wiki/CabalInstall" "cabal-install" %> this should get better, but, for now, installation is an adventure.
              </p>
              <p>
                In addition to its many other joys, the ORM in Turbinado only works with PostgreSQL right now.
              </p>
              <h1>Suit up</h1>
              <p>
                You'll need to have the following packages installed to have a go at installation:
              </p>
              <ul class="standard-list">
                <li><a href="http://www.haskell.org/ghc">GHC</a><em> (darcs) </em></li>
                <li><a href="http://code.haskell.org/HSP/haskell-src-exts/">haskell-src-exts</a><em> (darcs) </em></li>
                <li><a href="http://code.haskell.org/HSP/harp/">harp</a><em> (darcs) </em></li>
                <li><a href="http://git.complete.org/hslogger">hslogger</a><em> (git) </em></li>
                <li><a href="http://code.haskell.org/encoding/">encoding</a><em> (darcs) </em></li>
                <li><a href="http://code.haskell.org/HSP/hsx/">hsx</a><em> (darcs) </em></li>
                <li><a href="http://code.haskell.org/hs-plugins">hs-plugins</a><em> (darcs) </em></li>
                <li><a href="http://code.haskell.org/http">http</a><em> (darcs) </em></li>
                <li><a href="http://git.complete.org/hdbc">HDBC</a><em> (git) </em></li>
                <li><a href="http://git.complete.org/hdbc-postgresql">HDBC-PostgreSQL</a><em> (git) </em></li>
              </ul>
              <h1>Grab the code:</h1>
              <pre>
                git clone git://github.com/alsonkemp/turbinado.git
              </pre>
              <h1>Build it</h1>
              <p>
                With all of the packages installed, wait for a new moon, stand on tip-toes, and do the following:
              </p>
              <pre>
                runghc Setup.lhs configure
                runghc Setup.lhs build
              </pre>
              <p>
                If everything goes well, you should be able to do:
              </p>
              <pre>
                dist/build/turbinado/turbinado -p 9999
              </pre>
              <p>
                Try browsing to http://the-machines-name:9999/images/1x1.gif.
              </p>
            </div>
