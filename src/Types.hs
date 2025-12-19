module Types
  ( IrkFile (..),
    IrkFilePos (..),
    IrkFileArea (..),
    emptyFile,
    workspaceRoot,
  )
where

import System.OsPath (OsPath, unsafeEncodeUtf)

-- | Represents an area where a file can be looked for and
-- | found in.
data IrkFileArea
  = Workspace -- Files in a workspace (like LSP workspaces)
  | WorkspaceVendored -- Vendored files within a workspace
  | External -- Files external to all workspaces (global/system)
  deriving (Show, Eq)

-- | Represents a file or a directory.
data IrkFile = IrkFile
  { iPath :: OsPath,
    iFileSize :: Maybe Integer, -- If Nothing, then this is a directory
    iDepth :: Int, -- Depth from workspace root (0 is the workspace)
    iArea :: IrkFileArea
  }
  deriving (Show, Eq)

-- | Represents a text file position: path, line and column/char.
-- | Both values are 0-indexed, like in LSP.
data IrkFilePos = IrkFilePos IrkFile Int Int deriving (Show, Eq)

emptyFile :: IrkFile
emptyFile = IrkFile {iPath = unsafeEncodeUtf "", iFileSize = Just 0, iDepth = 0, iArea = Workspace}

workspaceRoot :: IrkFileArea -> OsPath -> IrkFile
workspaceRoot area path = IrkFile {iPath = path, iFileSize = Nothing, iDepth = 0, iArea = area}
