#!/bin/sh

cd /tmp
rm -rf hs-plugins-0.9.10*

darcs get --partial --set-scripts-executable /home/dons/hs-plugins
cd hs-plugins
rm -rf _darcs
cd ..
mv hs-plugins hs-plugins-0.9.10
tar czf hs-plugins-0.9.10.tar.gz hs-plugins-0.9.10
