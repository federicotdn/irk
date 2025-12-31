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

-- | Represents an area where an e.g. symbol can be looked for and found in.
data IrkFileArea
  = Workspace -- Files in a workspace (like LSP workspaces)
  | WorkspaceVendored -- Vendored files within a workspace
  | External -- Files external to all workspaces (global/system)
  deriving (Show, Eq)

-- | Represents a file or a directory.
data IrkFile = IrkFile
  { iPath :: OsPath, -- Complete path, usually absolute in LSP contexts.
    iRelPathParts :: [OsPath], -- Path parts relative to workspace root.
    iDir :: Bool,
    iFileSize :: Maybe Integer, -- Set to Nothing when this is a directory.
    iDepth :: Int, -- Depth from workspace root (0 is the workspace).
    iArea :: IrkFileArea
  }
  deriving (Show, Eq)

-- | Represents a text file position: path, line and column/char.
--  Both values are 0-indexed, like in LSP. The column/char value
--  indexes logical characters, not bytes (i.e. encoding-independent).
data IrkFilePos = IrkFilePos IrkFile Int Int deriving (Show, Eq)

file :: IrkFile
file =
  IrkFile
    { iPath = empty,
      iRelPathParts = [],
      iDir = False,
      iFileSize = Nothing,
      iDepth = 0,
      iArea = Workspace
    }

workspaceRoot :: IrkFileArea -> OsPath -> IrkFile
workspaceRoot area path =
  IrkFile
    { iPath = path,
      iRelPathParts = [],
      iDir = True,
      iFileSize = Nothing,
      iDepth = 0,
      iArea = area
    }
