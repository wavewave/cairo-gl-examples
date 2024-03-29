import Control.Concurrent (threadDelay)
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Data.Foldable (forM_) 
-- import Data.IORef
import qualified Data.Vector.Storable as SV hiding (forM_) 
import Foreign
import Foreign.C
import Graphics.Rendering.Cairo
import Graphics.UI.Gtk 
import Graphics.X11.Xlib as X11 
import Numeric (showHex)
-- 
import Graphics.Rendering.EGL.Raw
import Graphics.Rendering.EGL.Raw.Types 
-- 
import Prelude hiding (forM_)

kEGL_OPENGL_ES_BIT =		0x0001	
kEGL_OPENVG_BIT	   =		0x0002	
kEGL_OPENGL_ES2_BIT=		0x0004	
kEGL_OPENGL_BIT	   =		0x0008	



kEGL_BUFFER_SIZE =	        0x3020
kEGL_ALPHA_SIZE	=		0x3021
kEGL_BLUE_SIZE	=		0x3022
kEGL_GREEN_SIZE	=		0x3023
kEGL_RED_SIZE	=		0x3024
kEGL_DEPTH_SIZE	=		0x3025
kEGL_STENCIL_SIZE =	        0x3026
kEGL_CONFIG_CAVEAT =		0x3027
kEGL_CONFIG_ID	=		0x3028
kEGL_LEVEL	=		0x3029
kEGL_MAX_PBUFFER_HEIGHT =	0x302A
kEGL_MAX_PBUFFER_PIXELS	=	0x302B
kEGL_MAX_PBUFFER_WIDTH	=	0x302C
kEGL_NATIVE_RENDERABLE	=	0x302D
kEGL_NATIVE_VISUAL_ID	=	0x302E
kEGL_NATIVE_VISUAL_TYPE	=	0x302F
kEGL_SAMPLES		=	0x3031
kEGL_SAMPLE_BUFFERS	=	0x3032
kEGL_SURFACE_TYPE	=	0x3033
kEGL_TRANSPARENT_TYPE	=	0x3034
kEGL_TRANSPARENT_BLUE_VALUE =   0x3035
kEGL_TRANSPARENT_GREEN_VALUE=	0x3036
kEGL_TRANSPARENT_RED_VALUE  =	0x3037
kEGL_NONE		    =	0x3038
kEGL_BIND_TO_TEXTURE_RGB = 		0x3039
kEGL_BIND_TO_TEXTURE_RGBA=	0x303A
kEGL_MIN_SWAP_INTERVAL	 =	0x303B
kEGL_MAX_SWAP_INTERVAL	 =	0x303C
kEGL_LUMINANCE_SIZE	 =	0x303D
kEGL_ALPHA_MASK_SIZE	 =	0x303E
kEGL_COLOR_BUFFER_TYPE	 =	0x303F
kEGL_RENDERABLE_TYPE	 =	0x3040
kEGL_MATCH_NATIVE_PIXMAP =	0x3041	
kEGL_CONFORMANT		 =	0x3042



kEGL_SLOW_CONFIG	=	0x3050	
kEGL_NON_CONFORMANT_CONFIG=	0x3051	
kEGL_TRANSPARENT_RGB	=	0x3052	
kEGL_RGB_BUFFER		=	0x308E	
kEGL_LUMINANCE_BUFFER	=	0x308F	




kEGL_VENDOR  = 0x3053
kEGL_VERSION = 0x3054
kEGL_EXTENSION = 0x3055
kEGL_CLIENT_APIS = 0x308D


kEGL_CONTEXT_CLIENT_TYPE   =	0x3097
kEGL_CONTEXT_CLIENT_VERSION=	0x3098


kEGL_OPENGL_ES_API=		0x30A0
kEGL_OPENVG_API	  =		0x30A1
kEGL_OPENGL_API	  =		0x30A2


kEGL_HEIGHT	          = 	0x3056
kEGL_WIDTH		  =	0x3057
kEGL_LARGEST_PBUFFER	  = 	0x3058
kEGL_TEXTURE_FORMAT	  =	0x3080
kEGL_TEXTURE_TARGET	  =	0x3081
kEGL_MIPMAP_TEXTURE	  =	0x3082
kEGL_MIPMAP_LEVEL	  =	0x3083
kEGL_RENDER_BUFFER	  =	0x3086
kEGL_VG_COLORSPACE	  =	0x3087
kEGL_VG_ALPHA_FORMAT	  = 	0x3088
kEGL_HORIZONTAL_RESOLUTION=	0x3090
kEGL_VERTICAL_RESOLUTION  =	0x3091
kEGL_PIXEL_ASPECT_RATIO	  =	0x3092
kEGL_SWAP_BEHAVIOR	  =	0x3093
kEGL_MULTISAMPLE_RESOLVE  =	0x3099



main :: IO ()
main = do 
  initGUI
  Just (egldpy,x11dpy) <- runMaybeT $ do 
    gdkdpy <- MaybeT displayGetDefault
    str <- liftIO $ displayGetName gdkdpy 
    x11dpy <- liftIO $ X11.openDisplay str 
    let X11.Display x11dpyptr = x11dpy 
    eglnativedpytyp <- liftIO $ mkEGLNativeDisplayType (castPtr x11dpyptr)
    egldpy <- liftIO $ eglGetDisplay eglnativedpytyp
    return (egldpy,x11dpy)
  
  majorptr <- malloc 
  minorptr <- malloc
  eglInitialize egldpy majorptr minorptr
  putStrLn =<< peekCString =<< eglQueryString egldpy kEGL_VENDOR
  putStrLn =<< peekCString =<< eglQueryString egldpy kEGL_VERSION
  putStrLn =<< peekCString =<< eglQueryString egldpy kEGL_EXTENSION
  putStrLn =<< peekCString =<< eglQueryString egldpy kEGL_CLIENT_APIS

  p_eglconfig <- malloc 
  p_numconfig <- malloc 


  let attribs = SV.fromList [ kEGL_RED_SIZE,  1
                            , kEGL_GREEN_SIZE, 1 
                            , kEGL_BLUE_SIZE, 1
                            , kEGL_DEPTH_SIZE, 1 
                            , kEGL_RENDERABLE_TYPE, kEGL_OPENGL_ES2_BIT
                            , kEGL_NONE ] 
      ctxt_attribs = SV.fromList [ kEGL_CONTEXT_CLIENT_VERSION, 2 
                                 , kEGL_NONE ] 

  b <- SV.unsafeWith attribs $ \p_attrib -> 
         eglChooseConfig egldpy p_attrib p_eglconfig 1 p_numconfig
  print b 

  putStrLn . ("numconfig = " ++ ) . show =<< peek p_numconfig 

  eglconfig <- mkEGLConfig p_eglconfig 



  -- gtkroutine egldpy eglconfig eglctxt  


  let dflt = defaultScreen x11dpy
      border = blackPixel x11dpy dflt
      background = whitePixel x11dpy dflt
  rootw <- rootWindow x11dpy dflt
  win <- createSimpleWindow x11dpy rootw 0 0 100 100 1 border background
  setTextProperty x11dpy win "Hello World" wM_NAME
  mapWindow x11dpy win
  sync x11dpy False
  -- threadDelay (10 * 1000000)


  b2 <- eglBindAPI kEGL_OPENGL_ES_API
  print b2 

  nullEGLContext <- mkEGLContext nullPtr

  eglctxt <- SV.unsafeWith ctxt_attribs $ \p_ctxt_attrib -> 
          eglCreateContext egldpy eglconfig nullEGLContext p_ctxt_attrib


  p_testval <- malloc
  b3 <- eglQueryContext egldpy eglctxt kEGL_CONTEXT_CLIENT_VERSION p_testval 

  print b3 
  print =<< peek p_testval 

  eglsfc <- liftIO $ eglCreateWindowSurface egldpy eglconfig (fromIntegral win) nullPtr 

  err <- liftIO $ eglGetError 

  liftIO $ putStrLn ("errorcode = " ++ showHex err "" )

  let EGLSurface ptr = eglsfc 
  liftIO $ print ptr 
  let EGLConfig ptrcfg = eglconfig 
  liftIO $ putStrLn ("config = " ++ show ptrcfg) 


  p_testval <- liftIO $ malloc 
  b <- liftIO $ eglQuerySurface egldpy eglsfc kEGL_WIDTH p_testval 
  liftIO $  print b 
  liftIO $ print =<< peek p_testval 

 




  eglTerminate egldpy



  free majorptr 
  free minorptr  

  free p_eglconfig
  free p_numconfig 
  free p_testval 

gtkroutine egldpy eglconfig eglctxt = do
  window <- windowNew 
  vbox <- vBoxNew False 0 
  canvas <- drawingAreaNew 

  set canvas [ widgetWidthRequest := 400 
             , widgetHeightRequest := 400 ]   
  boxPackStart vbox canvas PackNatural 0   
  containerAdd window vbox 
  canvas `on` exposeEvent $ tryEvent $ do 
    win <- liftIO $ widgetGetDrawWindow canvas 
    nid <- liftIO $ drawableGetID win 
    liftIO $ print nid 
    liftIO $ print (fromNativeWindowId nid :: CULong)



    liftIO . renderWithDrawable win $ do 
      setSourceRGBA 0 0 0 1 
      setLineWidth 1.0
      moveTo 100 100 
      lineTo 200 200 
      stroke
  window `on` deleteEvent $ tryEvent $ do 
    liftIO $ mainQuit 
  widgetShowAll window
  mainGUI 


