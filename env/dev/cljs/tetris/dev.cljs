(ns ^:figwheel-no-load tetris.dev
  (:require
    [tetris.core :as core]
    [devtools.core :as devtools]))


(enable-console-print!)

(devtools/install!)

(core/init!)
