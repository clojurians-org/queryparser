{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module Database.Sql.Oracle.Type where

import GHC.Generics
import Data.Data hiding (DataType)
import Data.These

import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T
import qualified Data.Text.Lazy.Encoding as T


import Database.Sql.Position
  (Position(..), Range(..), advance, advanceHorizontal, advanceVertical)
import Database.Sql.Type
  ( RawNames(..), QSchemaName(..), ConstrainSNames(..), ConstrainSASNames(..)
  , mkNormalSchema)

-- https://docs.oracle.com/en/database/oracle/oracle-database/12.2/sqlqr/SQL-Statements.html
data OracleTotalStatement r a
  = OracleAlterClusterStatement (AlterCluster r a)
  | OracleAlterDatabaseStatement (AlterDatabase r a)
  | OracleAlterDatabaseLinkStatement (AlterDatabaseLink r a)
  | OracleAlterDimensionStatement (AlterDimension r a)
  | OracleAlterDiskgroupStatement (AlterDiskgroup r a)
  | OracleAlterFlashbackArchiveStatement (AlterFlashbackArchive r a)
  | OracleAlterFunctionStatement (AlterFunction r a)
  | OracleAlterIndexStatement (AlterIndex r a)
  | OracleAlterIndextypeStatement (AlterIndextype r a)
  | OracleAlterJavaStatement (AlterJava r a)
  | OracleAlterLibraryStatement (AlterLibrary r a)
  | OracleAlterMaterializedViewStatement (AlterMaterializedView r a)
  | OracleAlterMaterializedViewLogStatement (AlterMaterializedViewLog r a)
  | OracleAlterOperatorStatement (AlterOperator r a)
  | OracleAlterOutlineStatement (AlterOutline r a)
  | OracleAlterPackageStatement (AlterPackage r a)
  | OracleAlterProcedureStatement (AlterProcedure r a)
  | OracleAlterProfileStatement (AlterProfile r a)
  | OracleAlterResourceCostStatement (AlterResourceCost r a)
  | OracleAlterRoleStatement (AlterRole r a)
  | OracleAlterRollbackSegmentStatement (AlterRollbackSegment r a)
  | OracleAlterSequenceStatement (AlterSequence r a)
  | OracleAlterSessionStatement (AlterSession r a)
  | OracleAlterSystemStatement (AlterSystem r a)
  | OracleAlterTableStatement (AlterTable r a)
  | OracleAlterTablespaceStatement (AlterTablespace r a)
  | OracleAlterTriggerStatement (AlterTrigger r a)
  | OracleAlterTypeStatement (AlterType r a)
  | OracleAlterUserStatement (AlterUser r a)
  | OracleAlterViewStatement (AlterView r a)
  | OracleAnalyzeStatement (Analyze r a)
  | OracleAssociateStatisticsStatement (AssociateStatistics r a)
  | OracleAuditStatement (Audit r a)
  | OracleCallStatement (Call r a)
  | OracleCommentStatement (Comment r a)
  | OracleCommitStatement (Commit r a)
  | OracleCreateClusterStatement (CreateCluster r a)
  | OracleCreateContextStatement (CreateContext r a)
  | OracleCreateControlfileStatement (CreateControlfile r a)
  | OracleCreateDatabaseStatement (CreateDatabase r a)
  | OracleCreateDatabaseLinkStatement (CreateDatabaseLink r a)
  | OracleCreateDimensionStatement (CreateDimension r a)
  | OracleCreateDirectoryStatement (CreateDirectory r a)
  | OracleCreateDiskgroupStatement (CreateDiskgroup r a)
  | OracleCreateEditionStatement (CreateEdition r a)
  | OracleCreateFlashbackArchiveStatement (CreateFlashbackArchive r a)
  | OracleCreateFunctionStatement (CreateFunction r a)
  | OracleCreateIndexStatement (CreateIndex r a)
  | OracleCreateIndextypeStatement (CreateIndextype r a)
  | OracleCreateJavaStatement (CreateJava r a)
  | OracleCreateLibraryStatement (CreateLibrary r a)
  | OracleCreateMaterializedViewStatement (CreateMaterializedView r a)
  | OracleCreateMaterializedViewLogStatement (CreateMaterializedViewLog r a)
  | OracleCreateOperatorStatement (CreateOperator r a)
  | OracleCreateOutlineStatement (CreateOutline r a)
  | OracleCreatePackageStatement (CreatePackage r a)
  | OracleCreatePackageBodyStatement (CreatePackageBody r a)
  | OracleCreatePfileStatement (CreatePfile r a)
  | OracleCreateProcedureStatement (CreateProcedure r a)
  | OracleCreateProfileStatement (CreateProfile r a)
  | OracleCreateRestorePointStatement (CreateRestorePoint r a)
  | OracleCreateRoleStatement (CreateRole r a)
  | OracleCreateRollbackSegmentStatement (CreateRollbackSegment r a)
  | OracleCreateSchemaStatement (CreateSchema r a)
  | OracleCreateSequenceStatement (CreateSequence r a)
  | OracleCreateSpfileStatement (CreateSpfile r a)
  | OracleCreateSynonymStatement (CreateSynonym r a)
  | OracleCreateTableStatement (CreateTable r a)
  | OracleCreateTablespaceStatement (CreateTablespace r a)
  | OracleCreateTriggerStatement (CreateTrigger r a)
  | OracleCreateTypeStatement (CreateType r a)
  | OracleCreateTypeBodyStatement (CreateTypeBody r a)
  | OracleCreateUserStatement (CreateUser r a)
  | OracleCreateViewStatement (CreateView r a)
  | OracleDeleteStatement (Delete r a)
  | OracleDisassociateStatisticsStatement (DisassociateStatistics r a)
  | OracleDropClusterStatement (DropCluster r a)
  | OracleDropContextStatement (DropContext r a)
  | OracleDropDatabaseStatement (DropDatabase r a)
  | OracleDropDatabaseLinkStatement (DropDatabaseLink r a)
  | OracleDropDimensionStatement (DropDimension r a)
  | OracleDropDirectoryStatement (DropDirectory r a)
  | OracleDropDiskgroupStatement (DropDiskgroup r a)
  | OracleDropEditionStatement (DropEdition r a)
  | OracleDropFlashbackArchiveStatement (DropFlashbackArchive r a)
  | OracleDropFunctionStatement (DropFunction r a)
  | OracleDropIndexStatement (DropIndex r a)
  | OracleDropIndextypeStatement (DropIndextype r a)
  | OracleDropJavaStatement (DropJava r a)
  | OracleDropLibraryStatement (DropLibrary r a)
  | OracleDropMaterializedViewStatement (DropMaterializedView r a)
  | OracleDropMaterializedViewLogStatement (DropMaterializedViewLog r a)
  | OracleDropOperatorStatement (DropOperator r a)
  | OracleDropOutlineStatement (DropOutline r a)
  | OracleDropPackageStatement (DropPackage r a)
  | OracleDropProcedureStatement (DropProcedure r a)
  | OracleDropProfileStatement (DropProfile r a)
  | OracleDropRestorePointStatement (DropRestorePoint r a)
  | OracleDropRoleStatement (DropRole r a)
  | OracleDropRollbackSegmentStatement (DropRollbackSegment r a)
  | OracleDropSequenceStatement (DropSequence r a)
  | OracleDropSynonymStatement (DropSynonym r a)
  | OracleDropTableStatement (DropTable r a)
  | OracleDropTablespaceStatement (DropTablespace r a)
  | OracleDropTriggerStatement (DropTrigger r a)
  | OracleDropTypeStatement (DropType r a)
  | OracleDropTypeBodyStatement (DropTypeBody r a)
  | OracleDropUserStatement (DropUser r a)
  | OracleDropViewStatement (DropView r a)
  | OracleExplainPlanStatement (ExplainPlan r a)
  | OracleFlashbackDatabaseStatement (FlashbackDatabase r a)
  | OracleFlashbackTableStatement (FlashbackTable r a)
  | OracleGrantStatement (Grant r a)
  | OracleInsertStatement (Insert r a)
  | OracleLockTableStatement (LockTable r a)
  | OracleMergeStatement (Merge r a)
  | OracleNoauditStatement (Noaudit r a)
  | OraclePurgeStatement (Purge r a)
  | OracleRenameStatement (Rename r a)
  | OracleRevokeStatement (Revoke r a)
  | OracleRollbackStatement (Rollback r a)
  | OracleSavepointStatement (Savepoint r a)
  | OracleSelectStatement (Select r a)
  | OracleSetConstraintStatement (SetConstraint r a)
  | OracleSetRoleStatement (SetRole r a)
  | OracleSetTransactionStatement (SetTransaction r a)
  | OracleTruncateClusterStatement (TruncateCluster r a)
  | OracleTruncateTableStatement (TruncateTable r a)
  | OracleUpdateStatement (Update r a)
  | OracleUnhandledStatement a

deriving instance Generic (OracleTotalStatement r a)
deriving instance ConstrainSNames Show r a => Show (OracleTotalStatement r a)

data AlterCluster r a = AlterCluster a deriving (Generic, Show)

data AlterDatabase r a = AlterDatabase a deriving (Generic, Show, Eq)
data AlterDatabaseLink r a = AlterDatabaseLink a deriving (Generic, Show, Eq)
data AlterDimension r a = AlterDimension a deriving (Generic, Show, Eq)
data AlterDiskgroup r a = AlterDiskgroup a deriving (Generic, Show, Eq)
data AlterFlashbackArchive r a = AlterFlashbackArchive a deriving (Generic, Show, Eq)
data AlterFunction r a = AlterFunction a deriving (Generic, Show, Eq)
data AlterIndex r a = AlterIndex a deriving (Generic, Show, Eq)
data AlterIndextype r a = AlterIndextype a deriving (Generic, Show, Eq)
data AlterJava r a = AlterJava a deriving (Generic, Show, Eq)
data AlterLibrary r a = AlterLibrary a deriving (Generic, Show, Eq)
data AlterMaterializedView r a = AlterMaterializedView a deriving (Generic, Show, Eq)
data AlterMaterializedViewLog r a = AlterMaterializedViewLog a deriving (Generic, Show, Eq)
data AlterOperator r a = AlterOperator a deriving (Generic, Show, Eq)
data AlterOutline r a = AlterOutline a deriving (Generic, Show, Eq)
data AlterPackage r a = AlterPackage a deriving (Generic, Show, Eq)
data AlterProcedure r a = AlterProcedure a deriving (Generic, Show, Eq)
data AlterProfile r a = AlterProfile a deriving (Generic, Show, Eq)
data AlterResourceCost r a = AlterResourceCost a deriving (Generic, Show, Eq)
data AlterRole r a = AlterRole a deriving (Generic, Show, Eq)
data AlterRollbackSegment r a = AlterRollbackSegment a deriving (Generic, Show, Eq)
data AlterSequence r a = AlterSequence a deriving (Generic, Show, Eq)
data AlterSession r a = AlterSession a deriving (Generic, Show, Eq)
data AlterSystem r a = AlterSystem a deriving (Generic, Show, Eq)
data AlterTable r a = AlterTable a deriving (Generic, Show, Eq)
data AlterTablespace r a = AlterTablespace a deriving (Generic, Show, Eq)
data AlterTrigger r a = AlterTrigger a deriving (Generic, Show, Eq)
data AlterType r a = AlterType a deriving (Generic, Show, Eq)
data AlterUser r a = AlterUser a deriving (Generic, Show, Eq)
data AlterView r a = AlterView a deriving (Generic, Show, Eq)
data Analyze r a = Analyze a deriving (Generic, Show, Eq)
data AssociateStatistics r a = AssociateStatistics a deriving (Generic, Show, Eq)
data Audit r a = Audit a deriving (Generic, Show, Eq)
data Call r a = Call a deriving (Generic, Show, Eq)
data Comment r a = Comment a deriving (Generic, Show, Eq)
data Commit r a = Commit a deriving (Generic, Show, Eq)
data CreateCluster r a = CreateCluster a deriving (Generic, Show, Eq)
data CreateContext r a = CreateContext a deriving (Generic, Show, Eq)
data CreateControlfile r a = CreateControlfile a deriving (Generic, Show, Eq)
data CreateDatabase r a = CreateDatabase a deriving (Generic, Show, Eq)
data CreateDatabaseLink r a = CreateDatabaseLink a deriving (Generic, Show, Eq)
data CreateDimension r a = CreateDimension a deriving (Generic, Show, Eq)
data CreateDirectory r a = CreateDirectory a deriving (Generic, Show, Eq)
data CreateDiskgroup r a = CreateDiskgroup a deriving (Generic, Show, Eq)
data CreateEdition r a = CreateEdition a deriving (Generic, Show, Eq)
data CreateFlashbackArchive r a = CreateFlashbackArchive a deriving (Generic, Show, Eq)
data CreateFunction r a = CreateFunction a deriving (Generic, Show, Eq)
data CreateIndex r a = CreateIndex a deriving (Generic, Show, Eq)
data CreateIndextype r a = CreateIndextype a deriving (Generic, Show, Eq)
data CreateJava r a = CreateJava a deriving (Generic, Show, Eq)
data CreateLibrary r a = CreateLibrary a deriving (Generic, Show, Eq)
data CreateMaterializedView r a = CreateMaterializedView a deriving (Generic, Show, Eq)
data CreateMaterializedViewLog r a = CreateMaterializedViewLog a deriving (Generic, Show, Eq)
data CreateOperator r a = CreateOperator a deriving (Generic, Show, Eq)
data CreateOutline r a = CreateOutline a deriving (Generic, Show, Eq)
data CreatePackage r a = CreatePackage a deriving (Generic, Show, Eq)
data CreatePackageBody r a = CreatePackageBody a deriving (Generic, Show, Eq)
data CreatePfile r a = CreatePfile a deriving (Generic, Show, Eq)

-- https://docs.oracle.com/en/database/oracle/oracle-database/12.2/lnpls/CREATE-PROCEDURE-statement.html
data CreateProcedure r a = CreateProcedure
  { createProcedureInfo :: a
  , createProcedureOrReplace :: Maybe a
  , createProcedureName :: QProcedureName Maybe a
  , createProcedureParams :: [ParameterDeclaration r a]
  , createProcedureInvokerRights :: Maybe T.Text
  , createProcedureImpl :: CreateProcedureImpl r a
  }
deriving instance Generic (CreateProcedure r a)
deriving instance ConstrainSNames Show r a  => Show (CreateProcedure r a)

data CreateProfile r a = CreateProfile a deriving (Generic, Show, Eq)
data CreateRestorePoint r a = CreateRestorePoint a deriving (Generic, Show, Eq)
data CreateRole r a = CreateRole a deriving (Generic, Show, Eq)
data CreateRollbackSegment r a = CreateRollbackSegment a deriving (Generic, Show, Eq)
data CreateSchema r a = CreateSchema a deriving (Generic, Show, Eq)
data CreateSequence r a = CreateSequence a deriving (Generic, Show, Eq)
data CreateSpfile r a = CreateSpfile a deriving (Generic, Show, Eq)
data CreateSynonym r a = CreateSynonym a deriving (Generic, Show, Eq)
data CreateTable r a = CreateTable a deriving (Generic, Show, Eq)
data CreateTablespace r a = CreateTablespace a deriving (Generic, Show, Eq)
data CreateTrigger r a = CreateTrigger a deriving (Generic, Show, Eq)
data CreateType r a = CreateType a deriving (Generic, Show, Eq)
data CreateTypeBody r a = CreateTypeBody a deriving (Generic, Show, Eq)
data CreateUser r a = CreateUser a deriving (Generic, Show, Eq)
data CreateView r a = CreateView a deriving (Generic, Show, Eq)
data Delete r a = Delete a deriving (Generic, Show, Eq)
data DisassociateStatistics r a = DisassociateStatistics a deriving (Generic, Show, Eq)
data DropCluster r a = DropCluster a deriving (Generic, Show, Eq)
data DropContext r a = DropContext a deriving (Generic, Show, Eq)
data DropDatabase r a = DropDatabase a deriving (Generic, Show, Eq)
data DropDatabaseLink r a = DropDatabaseLink a deriving (Generic, Show, Eq)
data DropDimension r a = DropDimension a deriving (Generic, Show, Eq)
data DropDirectory r a = DropDirectory a deriving (Generic, Show, Eq)
data DropDiskgroup r a = DropDiskgroup a deriving (Generic, Show, Eq)
data DropEdition r a = DropEdition a deriving (Generic, Show, Eq)
data DropFlashbackArchive r a = DropFlashbackArchive a deriving (Generic, Show, Eq)
data DropFunction r a = DropFunction a deriving (Generic, Show)
data DropIndex r a = DropIndex a deriving (Generic, Show)
data DropIndextype r a = DropIndextype a deriving (Generic, Show)
data DropJava r a = DropJava a deriving (Generic, Show)
data DropLibrary r a = DropLibrary a deriving (Generic, Show)
data DropMaterializedView r a = DropMaterializedView a deriving (Generic, Show)
data DropMaterializedViewLog r a = DropMaterializedViewLog a deriving (Generic, Show)
data DropOperator r a = DropOperator a deriving (Generic, Show)
data DropOutline r a = DropOutline a deriving (Generic, Show)
data DropPackage r a = DropPackage a deriving (Generic, Show)
data DropProcedure r a = DropProcedure a deriving (Generic, Show)
data DropProfile r a = DropProfile a deriving (Generic, Show)
data DropRestorePoint r a = DropRestorePoint a deriving (Generic, Show)
data DropRole r a = DropRole a deriving (Generic, Show)
data DropRollbackSegment r a = DropRollbackSegment a deriving (Generic, Show)
data DropSequence r a = DropSequence a deriving (Generic, Show)
data DropSynonym r a = DropSynonym a deriving (Generic, Show)
data DropTable r a = DropTable a deriving (Generic, Show)
data DropTablespace r a = DropTablespace a deriving (Generic, Show)
data DropTrigger r a = DropTrigger a deriving (Generic, Show)
data DropType r a = DropType a deriving (Generic, Show)
data DropTypeBody r a = DropTypeBody a deriving (Generic, Show)
data DropUser r a = DropUser a deriving (Generic, Show)
data DropView r a = DropView a deriving (Generic, Show)
data ExplainPlan r a = ExplainPlan a deriving (Generic, Show)
data FlashbackDatabase r a = FlashbackDatabase a deriving (Generic, Show)
data FlashbackTable r a = FlashbackTable a deriving (Generic, Show)
data Grant r a = Grant a deriving (Generic, Show)
data Insert r a = Insert a deriving (Generic, Show)
data LockTable r a = LockTable a deriving (Generic, Show)
data Merge r a = Merge a deriving (Generic, Show)
data Noaudit r a = Noaudit a deriving (Generic, Show)
data Purge r a = Purge a deriving (Generic, Show)
data Rename r a = Rename a deriving (Generic, Show)
data Revoke r a = Revoke a deriving (Generic, Show)
data Rollback r a = Rollback a deriving (Generic, Show)
data Savepoint r a = Savepoint a deriving (Generic, Show)
data Select r a = Select a deriving (Generic, Show)
data SetConstraint r a = SetConstraint a deriving (Generic, Show)
data SetRole r a = SetRole a  deriving (Generic, Show)
data SetTransaction r a = SetTransaction a deriving (Generic, Show)
data TruncateCluster r a = TruncateCluster a deriving (Generic, Show)
data TruncateTable r a = TruncateTable a deriving (Generic, Show)
data Update r a = Update a deriving (Generic, Show)

data QProcedureName f a = QProcedureName
  { procedureNameInfo :: a
  , procedureNameSchema :: f (QSchemaName f a)
  , procedureNameName :: T.Text
  }
deriving instance Generic (QProcedureName f a)
deriving instance (Show a, Show (f (QSchemaName f a))) => Show (QProcedureName f a)

data Parameter a = Parameter T.Text a
  deriving (Generic, Show)
data BaseList b s = b :/ [s]
    deriving (Generic, Data, Eq, Ord, Show, Functor, Foldable, Traversable)

data ParameterDeclaration r a = ParameterDeclaration (Parameter a) (Maybe (ParameterBody r a))
deriving instance Generic (ParameterDeclaration r a)
deriving instance ConstrainSNames Show r a => Show (ParameterDeclaration r a)

data ParameterBody r a
  = ParameterBodyIn (DataType r a) (Maybe (Expression r a))
  | ParameterBodyInOut Bool (DataType r a)
deriving instance Generic (ParameterBody r a)
deriving instance ConstrainSNames Show r a => Show (ParameterBody r a)

data DataType r a
  = DataTypeColection a (CollectionType r a)
  | DataTypeObject a (ObjectType r a)
  | DataTypeRecord a (RecordType r a)
  | DataTypeCursor a (CursorType r a)
  | DataTypeRow a (RowType r a)
  | DataTypeScalar a ScalarType
  | DataTypeAttribute a (TypeAttribute r a)
deriving instance Generic (DataType r a)
deriving instance ConstrainSNames Show r a => Show (DataType r a)
deriving instance ConstrainSASNames Functor r => Functor (DataType r)
deriving instance ConstrainSASNames Foldable r => Foldable (DataType r)
deriving instance ConstrainSASNames Traversable r => Traversable (DataType r)

data CollectionType r a = CollectionType a deriving (Generic, Show, Functor, Foldable, Traversable)
data ObjectType r a = ObjectType a deriving (Generic, Show, Functor, Foldable, Traversable)
data RecordType r a = RecordType a deriving (Generic, Show, Functor, Foldable, Traversable)
data CursorType r a = CursorType a deriving (Generic, Show, Functor, Foldable, Traversable)
data RowType r a = RowType a deriving (Generic, Show, Functor, Foldable, Traversable)
data TypeAttribute r a = TypeAttribute a deriving (Generic, Show, Functor, Foldable, Traversable)

-- https://docs.oracle.com/cd/B28359_01/appdev.111/b28370/datatypes.htm#i43252
data ScalarType
  = PLS_INTEGER
  | BINARY_INTEGER
  | BINARY_FLOAT
  | BINARY_DOUBLE
  | NUMBER
  | CHAR
  | VARCHAR2
  | RAW
  | NCHAR
  | NVARCHAR2
  | LONG
  | LONG_RAW
  | ROWID
  | UROWID
  | CLOB
  | NCLOB
  deriving (Generic, Show)

data CreateProcedureImpl r a
    = ProcedureDeclareImpl a (Maybe (ProcedureDeclareSection r a)) (ProcedureBody r a)
    | ProcedureLanguageImpl a
deriving instance Generic (CreateProcedureImpl r a)
deriving instance ConstrainSNames Show r a => Show (CreateProcedureImpl r a)

data ProcedureDeclareSection r a
  = ProcedureDeclareSection (These [ProcedureItemList1Item r a] [ProcedureItemList2Item r a])
deriving instance Generic (ProcedureDeclareSection r a)
deriving instance ConstrainSNames Show r a => Show (ProcedureDeclareSection r a)
-- deriving instance ConstrainSASNames Functor r => Functor (ProcedureDeclareSection r)
-- deriving instance ConstrainSASNames Foldable r => Foldable (ProcedureDeclareSection r)
-- deriving instance ConstrainSASNames Traversable r => Traversable (ProcedureDeclareSection r)
instance Functor (ProcedureDeclareSection r) where
  fmap = undefined
instance Foldable (ProcedureDeclareSection r) where
  foldMap = undefined
instance Traversable (ProcedureDeclareSection r) where
  traverse = undefined

data ProcedureItemList1Item r a
    = ItemList1TypeDefinition a (TypeDefinition r a)
    | ItemList1CursorDeclaration a (CursorDeclaration r a)
    | ItemList1ItemDeclaration a (ItemDeclaration r a)
    | ItemList1FunctionDeclaration a (FunctionDeclaration r a)
    | ItemList1ProcedureDeclaration a (ProcedureDeclaration r a)
deriving instance Generic (ProcedureItemList1Item r a)
deriving instance ConstrainSNames Show r a => Show (ProcedureItemList1Item r a)
deriving instance ConstrainSASNames Functor r => Functor (ProcedureItemList1Item r)
deriving instance ConstrainSASNames Foldable r => Foldable (ProcedureItemList1Item r)
deriving instance ConstrainSASNames Traversable r => Traversable (ProcedureItemList1Item r)

data ProcedureItemList2Item r a
    = ItemList2CursorDeclaration a (CursorDeclaration r a)
    | ItemList2CursorDefinition a (CursorDefinition r a)
    | ItemList2FunctionDeclaration a (FunctionDeclaration r a)
    | ItemList2FunctionDefinition a (FunctionDefinition r a)
    | ItemList2ProcedureDeclaration a (ProcedureDeclaration r a)
    | ItemList2ProcedureDefinition a (ProcedureDefinition r a)
deriving instance Generic (ProcedureItemList2Item r a)
deriving instance ConstrainSNames Show r a => Show (ProcedureItemList2Item r a)
deriving instance ConstrainSASNames Functor r => Functor (ProcedureItemList2Item r)
deriving instance ConstrainSASNames Foldable r => Foldable (ProcedureItemList2Item r)
deriving instance ConstrainSASNames Traversable r => Traversable (ProcedureItemList2Item r)

data ProcedureBody r a = ProcedureBody
  { procedureBodyInfo :: a
  , procedureBodyStatements :: BaseList (ProcedureStatement r a) (ProcedureBodyStatement r a)
  , procedureExceptions :: [ProcedureExceptionHandler r a]
  } deriving (Generic, Data, Eq, Ord, Show)
instance Functor (ProcedureBody r) where
  fmap = undefined
instance Foldable (ProcedureBody r) where
  foldMap = undefined
instance Traversable (ProcedureBody r) where
  traverse = undefined

data ProcedureBodyStatement r a
    = ProcedureBodyStatement01 a (ProcedureStatement r a)
    | ProcedureBodyStatement02 a (ProcedureInlinePragma r a)
    deriving (Generic, Data, Eq, Ord, Show, Functor, Foldable, Traversable)

data Label a = Label T.Text
    deriving (Generic, Data, Eq, Ord, Show, Functor, Foldable, Traversable)
data ProcedureStatement r a
    = ProcedureStatement [Label a] (ProcedureStatementBase r a)
    deriving (Generic, Data, Eq, Ord, Show, Functor, Foldable, Traversable)
data ProcedureExceptionHandler r a = ProcedureExceptionHandler a
    deriving (Generic, Data, Eq, Ord, Show, Functor, Foldable, Traversable)
data ProcedureInlinePragma r a = ProcedureInlinePragma a
    deriving (Generic, Data, Eq, Ord, Show, Functor, Foldable, Traversable)
  
data Pragma r a = Pragma a
  deriving (Generic, Data, Eq, Ord, Show, Functor, Foldable, Traversable)

data TypeDefinition r a = TypeDefinition a
  deriving (Generic, Data, Eq, Ord, Show, Functor, Foldable, Traversable)
data ItemDeclaration r a
  = ItemCollectionVariableDeclaration (CollectionVariableDeclaration r a)
  | ItemConstantDeclaration (ConstantDeclaration r a)
  | ItemCursorVariableDeclaration (CursorVariableDeclaration r a)
  | ItemExceptionDeclaration (ExceptionDeclaration r a)
  | ItemRecordVariableDeclaration (RecordVariableDeclaration r a)
  | ItemVariableDeclaration (VariableDeclaration r a)
  
deriving instance Generic (ItemDeclaration r a)
deriving instance ConstrainSNames Show r a => Show (ItemDeclaration r a)
deriving instance ConstrainSASNames Functor r => Functor (ItemDeclaration r)
deriving instance ConstrainSASNames Foldable r => Foldable (ItemDeclaration r)
deriving instance ConstrainSASNames Traversable r => Traversable (ItemDeclaration r)

data CursorDeclaration r a = CursorDeclaration a
  deriving (Generic, Data, Eq, Ord, Show, Functor, Foldable, Traversable)
data CursorDefinition r a = CursorDefinition a
  deriving (Generic, Data, Eq, Ord, Show, Functor, Foldable, Traversable)
data FunctionDeclaration r a = FunctionDeclaration a
  deriving (Generic, Data, Eq, Ord, Show, Functor, Foldable, Traversable)
data FunctionDefinition r a = FunctionDefinition a
  deriving (Generic, Data, Eq, Ord, Show, Functor, Foldable, Traversable)
data ProcedureDeclaration r a = ProcedureDeclaration a
  deriving (Generic, Data, Eq, Ord, Show, Functor, Foldable, Traversable)
data ProcedureDefinition r a = ProcedureDefinition a
  deriving (Generic, Data, Eq, Ord, Show, Functor, Foldable, Traversable)

data CollectionVariableDeclaration r a = CollectionVariableDeclaration a
  deriving (Generic, Data, Eq, Ord, Show, Functor, Foldable, Traversable)
data ConstantDeclaration r a = ConstantDeclaration a 
  deriving (Generic, Data, Eq, Ord, Show, Functor, Foldable, Traversable)
data CursorVariableDeclaration r a = CursorVariableDeclaration a
  deriving (Generic, Data, Eq, Ord, Show, Functor, Foldable, Traversable)
data ExceptionDeclaration r a = ExceptionDeclaration a
  deriving (Generic, Data, Eq, Ord, Show, Functor, Foldable, Traversable)
data RecordVariableDeclaration r a = RecordVariableDeclaration a
  deriving (Generic, Data, Eq, Ord, Show, Functor, Foldable, Traversable)
data VariableDeclaration r a = VariableDeclaration
  { variableInfo :: a
  , variableName :: T.Text
  , dataType :: DataType r a
  , variableImpl :: Maybe (VariableDeclarationContext r a)
  }

deriving instance Generic (VariableDeclaration r a)
deriving instance ConstrainSNames Show r a => Show (VariableDeclaration r a)
deriving instance ConstrainSASNames Functor r => Functor (VariableDeclaration r)
deriving instance ConstrainSASNames Foldable r => Foldable (VariableDeclaration r)
deriving instance ConstrainSASNames Traversable r => Traversable (VariableDeclaration r)

data VariableDeclarationContext r a = VariableDeclarationContext
  { variableDeclarationContextInfo :: a
  , variableDeclarationContextNullable :: Bool
  , variableDeclarationContextExpression :: Expression r a
  }
deriving instance Generic (VariableDeclarationContext r a)
deriving instance ConstrainSNames Show r a => Show (VariableDeclarationContext r a)
deriving instance ConstrainSASNames Functor r => Functor (VariableDeclarationContext r)
deriving instance ConstrainSASNames Foldable r => Foldable (VariableDeclarationContext r)
deriving instance ConstrainSASNames Traversable r => Traversable (VariableDeclarationContext r)


data ProcedureStatementBase r a
  = ProcedureStatementBase01 (AssignmentStatement r a)
{--
  | ProcedureStatementBase02 (BasicLoopStatement r a)
  | CaseStatement r a
  | CloseStatement r a
  | CollectionMethodCall2 r a
  | ContinueStatement r a
  | CursorForLoopStatement r a
  | ExecuteImmediateStatement r a
  | ExitStatement r a
  | FetchStatement r a
  | ForLoopStatement r a
  | ForallStatement r a
  | GotoStatement r a
  | IfStatement r a
  | NullStatement r a
  | OpenStatement r a
  | OpenForStatement r a
  | PipeRowStatement r a
  | PlsqlBlock r a
  | ProcedureCall r a
  | RaiseStatement r a
  | ReturnStatement r a
  | SelectIntoStatement r a
  | SqlStatement r a
  | WhileLoopStatement r a
--}
  deriving (Generic, Data, Eq, Ord, Show, Functor, Foldable, Traversable)
    
data SqlStatement r a
  = CommitStatement r a
  | CollectionMethodCall r a
  | DeleteStatement r a
  | InsertStatement r a
  | LockTableStatement r a
  | MergeStatement r a
  | RollbackStatement r a
  | SavepointStatement r a
  | SetTransactionStatement r a
  | UpdateStatement r a
  deriving (Generic, Data, Eq, Ord, Show, Functor, Foldable, Traversable)

data AssignmentStatement r a
  = AssignmentStatement a (AssignStatementTarget r a) (Expression r a)
  deriving (Generic, Data, Eq, Ord, Show, Functor, Foldable, Traversable)

data AssignStatementTarget r a = AssignStatementTarget a
  deriving (Generic, Data, Eq, Ord, Show, Functor, Foldable, Traversable)

data Expression r a
  = Expression01 a (BooleanExpression r a)
  | Expression02 a (CharacterExpression r a)
  | Expression03 a (CollectionConstructor r a)
  | Expression04 a (DateExpression r a)
  | Expression05 a (NumericExpression r a)
  | Expression06 a (SearchedCaseExpression r a)
  | Expression07 a (SimpleCaseExpression r a)
  | Expression08 a (Expression r a)
  deriving (Generic, Data, Eq, Ord, Show, Functor, Foldable, Traversable)

data BooleanExpression r a = BooleanExpression a
  deriving (Generic, Data, Eq, Ord, Show, Functor, Foldable, Traversable)
data CharacterExpression r a = CharacterExpression a
  deriving (Generic, Data, Eq, Ord, Show, Functor, Foldable, Traversable)
data CollectionConstructor r a = CollectionConstructor a
  deriving (Generic, Data, Eq, Ord, Show, Functor, Foldable, Traversable)
data DateExpression r a = DateExpression a
  deriving (Generic, Data, Eq, Ord, Show, Functor, Foldable, Traversable)
data NumericExpression r a = NumericExpression a
  deriving (Generic, Data, Eq, Ord, Show, Functor, Foldable, Traversable)
data SearchedCaseExpression r a = SearchedCaseExpression a
  deriving (Generic, Data, Eq, Ord, Show, Functor, Foldable, Traversable)
data SimpleCaseExpression r a = SimpleCaseExpression a
  deriving (Generic, Data, Eq, Ord, Show, Functor, Foldable, Traversable)
