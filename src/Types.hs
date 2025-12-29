module Types
  ( IrkFile (..),
    IrkFilePos (..),
    IrkFileArea (..),
    file,
    workspaceRoot,
  )
where

import System.OsPath (OsPath)
import System.OsString (empty)

-- |Represents an area where an e.g. symbol can be looked for and found in.
data IrkFileArea
  = Workspace -- Files in a workspace (like LSP workspaces)
  | WorkspaceVendored -- Vendored files within a workspace
  | External -- Files external to all workspaces (global/system)
  deriving (Show, Eq)

-- |Represents a file or a directory.
data IrkFile = IrkFile
  { iPath :: OsPath,
    iDir :: Bool,
    iFileSize :: Maybe Integer, -- If Nothing, then this is a directory
    iDepth :: Int, -- Depth from workspace root (0 is the workspace)
    iArea :: IrkFileArea
  }
  deriving (Show, Eq)

-- |Represents a text file position: path, line and column/char.
-- Both values are 0-indexed, like in LSP. UTF32 position encoding
-- is assumed.
data IrkFilePos = IrkFilePos IrkFile Int Int deriving (Show, Eq)

file :: IrkFile
file = IrkFile {iPath = empty, iDir = False, iFileSize = Nothing, iDepth = 0, iArea = Workspace}

workspaceRoot :: IrkFileArea -> OsPath -> IrkFile
workspaceRoot area path = IrkFile {iPath = path, iDir = True, iFileSize = Nothing, iDepth = 0, iArea = area}
