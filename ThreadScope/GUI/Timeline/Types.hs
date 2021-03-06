module GUI.Timeline.Types (
    TimelineState(..),
 ) where


import GUI.Types

import Graphics.UI.Gtk
import Graphics.Rendering.Cairo
import Data.IORef

-----------------------------------------------------------------------------

data TimelineState = TimelineState {
       timelineDrawingArea      :: DrawingArea,
       timelineLabelDrawingArea :: DrawingArea,
       timelineAdj              :: Adjustment,
       timelineVAdj             :: Adjustment,

       timelinePrevView  :: IORef (Maybe (ViewParameters, Surface)),

       scaleIORef        :: IORef Double -- in ns/pixel
     }

-----------------------------------------------------------------------------
