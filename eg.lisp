(load "tar3lib")

(opt "-h" "TAR3: recursive sampler
           (c) 2022 Tim Menzies <timm@ieee.org> BSD-2" 'help  nil)
(opt "-g" "start-up action; all= run all" 'go   "none")
(opt "-p" "whatever floats your boat    " 'p    2)
(opt "-s" "random number seed           " 'seed 10013)

(cli)
(run)
