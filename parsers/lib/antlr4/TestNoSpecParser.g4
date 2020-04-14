parser grammar TestNoSpecParser;

valEmp: EOF EOF;

valContractClause: valEmp;
valStatement: valEmp;
valWithThen: valEmp;
valPrimary: valEmp;
valReserved: valEmp;
valType: valEmp;
valDeclaration: valEmp;
valModifier: valEmp;
valEmbedContract: valEmp;
valEmbedStatementBlock: valEmp;
valEmbedWithThenBlock: valEmp;
valEmbedWithThen: valEmp;
valEmbedDeclarationBlock: valEmp;
valMulOp: valEmp;
valAndOp: valEmp;
valImpOp: valEmp;