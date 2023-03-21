#
# Script that creates the implementation files
# for sc_signal and sc_fifo
#

sc2ast=./../../../../SC2AST/bin/sc2ast.jar


function createAst {
	cp $1 $1.tmp
	sed -i .bak -e 's/GTYPE/char/g' $1.tmp
	rm $1.tmp.bak
	
	java -jar $sc2ast -f $1.tmp -o $1_ast
	sed -i .bak -e 's/char/GTYPE/g' $1_ast.ast.xml
	sed -i .bak -e 's/$2x/$2/g' $1_ast.ast.xml
	
	mv $1_ast.ast.xml $1.ast.xml
	rm $1.tmp
	rm $1_ast
	rm $1_ast.ast.xml.bak
}

function createAstGeneric {
	cp $1 $1.tmp
	sed -i .bak -e 's/GTYPE/sc_uint<13>/g' $1.tmp
	rm $1.tmp.bak
	
	java -jar $sc2ast -f $1.tmp -o $1_ast
	sed -i .bak -e 's/sc_uint/GTYPE/g' $1_ast.ast.xml
	sed -i .bak -e 's/length="13"/length="GLENGTH"/g' $1_ast.ast.xml
	sed -i .bak -e 's/$2x/$2/g' $1_ast.ast.xml
	
	mv $1_ast.ast.xml $1_generic.ast.xml
	rm $1.tmp
	rm $1_ast
	rm $1_ast.ast.xml.bak
}



createAst ./implementation/sc_signal/sc_signal sc_signal
createAstGeneric ./implementation/sc_signal/sc_signal sc_signal

createAst ./implementation/sc_fifo/sc_fifo sc_fifo
createAstGeneric ./implementation/sc_fifo/sc_fifo sc_fifo

