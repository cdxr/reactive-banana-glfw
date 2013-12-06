module Reactive.Banana.GLFW.Behaviors where

import Reactive.Banana.GLFW.Window hiding ( size )

import Reactive.Banana

import Reactive.Banana.GLFW.Events



cursorPos :: WindowEvents t -> Behavior t (Maybe (Double, Double))
cursorPos = stepper Nothing . cursorMove'
