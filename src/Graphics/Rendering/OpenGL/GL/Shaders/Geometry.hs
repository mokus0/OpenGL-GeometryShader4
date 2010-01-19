module Graphics.Rendering.OpenGL.GL.Shaders.Geometry (
   -- * Shader Objects
   GeometryShader, geometryShaderDeleteStatus, geometryShaderSource,
   compileGeometryShader, geometryShaderCompileStatus, geometryShaderInfoLog,

   -- * Program Objects
   attachedShaders,
   
   geometryVerticesOut,
   GeometryInputType(..), GeometryOutputType(..),
   geometryInputType, geometryOutputType
) where

import Control.Monad
import Data.List
import Data.ObjectName
import Data.StateVar
import Foreign.C.String
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable
import Graphics.Rendering.OpenGL.GL.Shaders hiding (attachedShaders)
import Graphics.Rendering.OpenGL.Raw.Core31
import Graphics.Rendering.OpenGL.Raw.ARB.GeometryShader4
import Unsafe.Coerce

newtype GeometryShader = GeometryShader { geometryShaderID :: GLuint }
   deriving ( Eq, Ord, Show )

--------------------------------------------------------------------------------

instance ObjectName GeometryShader where
   genObjectNames = genShaderNames
   deleteObjectNames = deleteShaderNames
   isObjectName = isShaderName

genShaderNames :: Int -> IO [GeometryShader]
genShaderNames n = replicateM n createShader

createShader :: IO GeometryShader
createShader = fmap GeometryShader $ glCreateShader gl_GEOMETRY_SHADER

deleteShaderNames :: [GeometryShader] -> IO ()
deleteShaderNames = mapM_ (glDeleteShader . geometryShaderID)

isShaderName :: GeometryShader -> IO Bool
isShaderName = fmap unmarshalGLboolean . glIsShader . geometryShaderID

unmarshalGLboolean :: Num a => a -> Bool
unmarshalGLboolean = (/= fromIntegral gl_FALSE)

--------------------------------------------------------------------------------

compileGeometryShader :: GeometryShader -> IO ()
compileGeometryShader = glCompileShader . geometryShaderID

--------------------------------------------------------------------------------

geometryShaderSource shader =
   makeStateVar (getShaderSource shader) (setShaderSource shader)

setShaderSource shader srcs = do
   let len = genericLength srcs
   withMany withGLStringLen srcs $ \charBufsAndLengths -> do
      let (charBufs, lengths) = unzip charBufsAndLengths
      withArray charBufs $ \charBufsBuf ->
         withArray (map fromIntegral lengths) $ \lengthsBuf ->
            glShaderSource (geometryShaderID shader) len charBufsBuf lengthsBuf

getShaderSource shader = do
   src <- get (stringQuery (shaderSourceLength shader)
                           (glGetShaderSource (geometryShaderID shader)))
   return [src]


peekGLstringLen :: (Ptr GLchar, GLsizei) -> IO String
peekGLstringLen (p,l) = peekCAStringLen (castPtr p, fromIntegral l)

withGLStringLen :: String -> ((Ptr GLchar, GLsizei) -> IO a) -> IO a
withGLStringLen s act =
   withCAStringLen s $ \(p,len) ->
      act (castPtr p, fromIntegral len)


stringQuery :: GettableStateVar GLsizei -> (GLsizei -> Ptr GLsizei -> Ptr GLchar -> IO ()) -> GettableStateVar String
stringQuery lengthVar getStr =
   makeGettableStateVar $ do
      len <- get lengthVar -- Note: This includes the NUL character!
      if len == 0
        then return ""
        else allocaArray (fromIntegral len) $ \buf -> do
                getStr len nullPtr buf
                peekGLstringLen (buf, len-1)


--------------------------------------------------------------------------------

geometryShaderDeleteStatus :: GeometryShader -> GettableStateVar Bool
geometryShaderDeleteStatus = shaderVar unmarshalGLboolean ShaderDeleteStatus

geometryShaderCompileStatus :: GeometryShader -> GettableStateVar Bool
geometryShaderCompileStatus = shaderVar unmarshalGLboolean CompileStatus

geometryShaderInfoLogLength :: GeometryShader -> GettableStateVar GLsizei
geometryShaderInfoLogLength = shaderVar fromIntegral ShaderInfoLogLength

shaderSourceLength :: GeometryShader -> GettableStateVar GLsizei
shaderSourceLength = shaderVar fromIntegral ShaderSourceLength

shaderTypeEnum :: GLuint -> GettableStateVar GLenum
shaderTypeEnum shader =
   makeGettableStateVar $
      alloca $ \buf -> do
         glGetShaderiv shader gl_SHADER_TYPE buf
         peek1 fromIntegral buf

--------------------------------------------------------------------------------

data GetShaderPName =
     ShaderDeleteStatus
   | CompileStatus
   | ShaderInfoLogLength
   | ShaderSourceLength
   | ShaderType

marshalGetShaderPName :: GetShaderPName -> GLenum
marshalGetShaderPName x = case x of
   ShaderDeleteStatus -> gl_DELETE_STATUS
   CompileStatus -> gl_COMPILE_STATUS
   ShaderInfoLogLength -> gl_INFO_LOG_LENGTH
   ShaderSourceLength -> gl_SHADER_SOURCE_LENGTH
   ShaderType -> gl_SHADER_TYPE

shaderVar :: (GLint -> a) -> GetShaderPName -> GeometryShader -> GettableStateVar a
shaderVar f p shader =
   makeGettableStateVar $
      alloca $ \buf -> do
         glGetShaderiv (geometryShaderID shader) (marshalGetShaderPName p) buf
         peek1 f buf

{-# INLINE peek1 #-}
peek1 :: Storable a => (a -> b) -> Ptr a -> IO b
peek1 f ptr = do
   x <- peekElemOff ptr 0
   return $ f x

--------------------------------------------------------------------------------

geometryShaderInfoLog :: GeometryShader -> GettableStateVar String
geometryShaderInfoLog shader =
   stringQuery (geometryShaderInfoLogLength shader) (glGetShaderInfoLog (geometryShaderID shader))

--------------------------------------------------------------------------------

attachedShaders :: Program -> StateVar ([VertexShader],[FragmentShader],[GeometryShader])
attachedShaders program =
   makeStateVar (getAttachedShaders program) (setAttachedShaders program)

getAttachedShaders :: Program -> IO ([VertexShader],[FragmentShader],[GeometryShader])
getAttachedShaders program = getAttachedShaderIDs program >>= splitShaderIDs

getAttachedShaderIDs :: Program -> IO [GLuint]
getAttachedShaderIDs program = do
   numShaders <- get (numAttachedShaders program)
   allocaArray (fromIntegral numShaders) $ \buf -> do
      glGetAttachedShaders (programID program) numShaders nullPtr buf
      peekArray (fromIntegral numShaders) buf

splitShaderIDs :: [GLuint] -> IO ([VertexShader],[FragmentShader],[GeometryShader])
splitShaderIDs ids = do
   (vs, others) <- partitionM isVertexShaderID ids
   (fs, gs)     <- partitionM isFragmentShaderID others
   return (map toVertexShader vs, map toFragmentShader fs, map GeometryShader gs)

isVertexShaderID :: GLuint -> IO Bool
isVertexShaderID x = do
   t <- get (shaderTypeEnum x)
   return $ t == gl_VERTEX_SHADER

isFragmentShaderID :: GLuint -> IO Bool
isFragmentShaderID x = do
   t <- get (shaderTypeEnum x)
   return $ t == gl_FRAGMENT_SHADER

partitionM :: (a -> IO Bool) -> [a] -> IO ([a],[a])
partitionM p = foldM select ([],[])
   where select (ts, fs) x = do
            b <- p x
            return $ if b then (x:ts, fs) else (ts, x:fs)

setAttachedShaders :: Program -> ([VertexShader],[FragmentShader],[GeometryShader]) -> IO ()
setAttachedShaders p (vs, fs, gs) = do
   currentIDs <- getAttachedShaderIDs p
   let newIDs = map vertexShaderID vs ++ map fragmentShaderID fs ++ map geometryShaderID gs
       program = programID p
   mapM_ (glAttachShader program) (newIDs \\ currentIDs)
   mapM_ (glDetachShader program) (currentIDs \\ newIDs)

toVertexShader :: GLuint -> VertexShader
toVertexShader = unsafeCoerce
toFragmentShader :: GLuint -> FragmentShader
toFragmentShader = unsafeCoerce
vertexShaderID :: VertexShader -> GLuint
vertexShaderID = unsafeCoerce
fragmentShaderID :: FragmentShader -> GLuint
fragmentShaderID = unsafeCoerce

--------------------------------------------------------------------------------

numAttachedShaders program =
   makeGettableStateVar $
      alloca $ \buf -> do
         glGetProgramiv (programID program) gl_ATTACHED_SHADERS buf
         peek1 fromIntegral buf

programID :: Program -> GLuint
programID = unsafeCoerce

--------------------------------------------------------------------------------

-- extra program state

geometryVerticesOut :: Program -> StateVar GLuint
geometryVerticesOut program = 
   makeStateVar (getGeometryVerticesOut program) (setGeometryVerticesOut program)

getGeometryVerticesOut program = 
    alloca $ \buf -> do
         glGetProgramiv (programID program) gl_GEOMETRY_VERTICES_OUT buf
         peek1 fromIntegral buf

setGeometryVerticesOut p n = glProgramParameteri (programID p) gl_GEOMETRY_VERTICES_OUT (fromIntegral n)


--------------------------------------------------------------------------------

geometryInputType :: Program -> StateVar GeometryInputType
geometryInputType p = makeStateVar get set
    where
        get = alloca $ \buf -> do
            glGetProgramiv (programID p) gl_GEOMETRY_INPUT_TYPE (castPtr buf)
            peek1 unmarshalInputGeometryType buf
        set = glProgramParameteri (programID p) gl_GEOMETRY_INPUT_TYPE . fromIntegral . marshalGeometryInputType

data GeometryInputType
    = InPoints
    | InLines
    | InLinesAdjacency
    | InTriangles
    | InTrianglesAdjacency
    deriving (Eq, Ord, Read, Show)

marshalGeometryInputType :: GeometryInputType -> GLenum
marshalGeometryInputType x = case x of
   InPoints -> gl_POINTS
   InLines -> gl_LINES
   InLinesAdjacency -> gl_LINES_ADJACENCY
   InTriangles -> gl_TRIANGLES
   InTrianglesAdjacency -> gl_TRIANGLES_ADJACENCY
   

unmarshalInputGeometryType :: GLenum -> GeometryInputType
unmarshalInputGeometryType x
   | x == gl_POINTS = InPoints
   | x == gl_LINES = InLines
   | x == gl_LINES_ADJACENCY = InLinesAdjacency
   | x == gl_TRIANGLES = InTriangles
   | x == gl_TRIANGLES_ADJACENCY = InTrianglesAdjacency
   | otherwise = error ("unmarshalGeomotryType: illegal value " ++ show x)


--------------------------------------------------------------------------------

geometryOutputType :: Program -> StateVar GeometryOutputType
geometryOutputType p = makeStateVar get set
    where
        get = alloca $ \buf -> do
            glGetProgramiv (programID p) gl_GEOMETRY_OUTPUT_TYPE (castPtr buf)
            peek1 unmarshalGeometryOutputType buf
        set = glProgramParameteri (programID p) gl_GEOMETRY_OUTPUT_TYPE . fromIntegral . marshalGeometryOutputType

data GeometryOutputType
    = OutPoints
    | OutLineStrip
    | OutTriangleStrip
    deriving (Eq, Ord, Read, Show)

marshalGeometryOutputType :: GeometryOutputType -> GLenum
marshalGeometryOutputType x = case x of
   OutPoints -> gl_POINTS
   OutLineStrip -> gl_LINE_STRIP
   OutTriangleStrip -> gl_TRIANGLE_STRIP

unmarshalGeometryOutputType :: GLenum -> GeometryOutputType
unmarshalGeometryOutputType x
   | x == gl_POINTS = OutPoints
   | x == gl_LINE_STRIP = OutLineStrip
   | x == gl_TRIANGLE_STRIP = OutTriangleStrip
   | otherwise = error ("unmarshalGeomotryType: illegal value " ++ show x)


