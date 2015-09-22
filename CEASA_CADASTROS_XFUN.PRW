/*
+----------------------------------------------------------------------------+
!                        FICHA TECNICA DO PROGRAMA                           !
+----------------------------------------------------------------------------+
! DADOS DO PROGRAMA 														 !
+------------------+---------------------------------------------------------+
!Tipo 			   ! Atualização 											 !
+------------------+---------------------------------------------------------+
!Modulo 		   ! CADASTROS	 											 !
+------------------+---------------------------------------------------------+
!Nome 			   ! CEASA_CADASTROS_XFUN.PRW								 !
+------------------+---------------------------------------------------------+
!Descricao 		   ! Funções para tratamento de cadastros					 !
+------------------+---------------------------------------------------------+
!Autor 			   ! Gilson Lima											 !
+------------------+---------------------------------------------------------+
!Data de Criacao   ! 31/08/2015												 !
+------------------+---------------------------------------------------------+
! ATUALIZACOES 	   															 !
+-------------------------------------------+-----------+-----------+--------+
! Descricao detalhada da atualizacao 		!Nome do    ! Analista  !Data da !
! 											!Solicitante! Respons.  !Atualiz.!
+-------------------------------------------+-----------+-----------+--------+
!  									 		! 		 	! 		 	!		 !
! 											! 		 	! 			! 		 !
+-------------------------------------------+-----------+-----------+--------+
*/

#include "Protheus.ch"

/*----------+-----------+-------+--------------------+------+----------------+
! Programa 	! GeraCod	! Autor !Gilson Lima 		 ! Data ! 31/08/2015     !
+-----------+-----------+-------+--------------------+------+----------------+
! Descricao ! Funcao auxiliar de geração de código do fornecedor baseado	 !
! 			! no CNPF definindo automaticamente a loja						 !
+----------------------------------------------------------------------------*/
User Function GeraCod(cDoc,cTab)

	Local aArea		:= GetArea()
	Local cDoc		:= AllTrim(cDoc)
	Local cCod		:= ''
	Local cLoja		:= ''
	Local cTipo		:= IIF(Len(cDoc) == 14, 'J', IIF(Len(cDoc) == 11, 'F', 'X'))
	Local cQuery 	:= ''
	Local nReg		:= 0
	Local nReg2		:= 0
	
	Local cCpCod	:= cTab + "." + Substr(cTab,2,2) + "_COD"
	Local cCpDoc	:= cTab + "." + Substr(cTab,2,2) + "_CGC"
	Local cCpLoja	:= cTab + "." + Substr(cTab,2,2) + "_LOJA"
	Local cCpFil	:= cTab + "." + Substr(cTab,2,2) + "_FILIAL"
	
	// Pessoa Jurídica
	If Len(cDoc) == 14
		
		// Verifica se já existe o CNPJ na Base
		cQuery := "SELECT " + cCpCod + " FROM " + RetSqlName(cTab) + " " + cTab
		cQuery += " WHERE " + cTab + ".D_E_L_E_T_ <> '*'"
		cQuery += " AND SUBSTRING(" + cCpDoc + ",1,8) = '" + SUBSTR(cDoc,1,8) + "'"
		cQuery += " AND " + cCpFil + " = '" + xFilial(cTab) + "'"
		
		If Select('TRB') <> 0
			dbSelectArea('TRB')
			TRB->(dbCloseArea())
		EndIf
		
		dbUseArea(.T.,"TOPCONN",TCGenQry(,,cQuery),'TRB',.F.,.F.)
		
		nReg := Contar("TRB","!EOF()")
		
		If nReg > 0
			
			dbSelectArea('TRB')
			TRB->(dbGoTop())
			
			cCod  := TRB->&(Substr(cCpCod,5,2)+"_COD")
			cLoja := Substr(cDoc,9,4)
			
			cQuery := "SELECT " + cCpCod + ", " + cCpFil + " FROM " + RetSqlName(cTab) + " " + cTab
			cQuery += " WHERE " + cTab + ".D_E_L_E_T_ <> '*'"
			cQuery += " AND " + cCpFil + " = '" + xFilial(cTab) + "'"
			cQuery += " AND " + cCpCod + " = '" + cCod + "'"
			cQuery += " AND " + cCpLoja + " = '" + cLoja + "'"
			
			If Select('TRB2') <> 0
				dbSelectArea('TRB2')
				TRB2->(dbCloseArea())
			EndIf
			
			dbUseArea(.T.,"TOPCONN",TCGenQry(,,cQuery),'TRB2',.F.,.F.)
			
			nReg2 := Contar("TRB2","!EOF()")
			
			If nReg2 > 0
				TRB2->(dbCloseArea())
				Alert("Cadastro já existente na base.")
				Return
			EndIf
			
			TRB2->(dbCloseArea())
		
		EndIf
		
		TRB->(dbCloseArea())
	
	Else
	
		cQuery := "SELECT " + cCpCod + ", " + cCpLoja + " FROM " + RetSqlName(cTab) + " " +cTab
		cQuery += " WHERE " + cTab + ".D_E_L_E_T_ <> '*'"
		cQuery += " AND " + cCpDoc + " = '" + PadR(AllTrim(cDoc),TamSX3(Substr(cCpDoc,5))[1], ' ') + "'"
		cQuery += " AND " + cCpFil + " = '" + xFilial(cTab) + "'"
		
		If Select('TRB') <> 0
			dbSelectArea('TRB')
			TRB->(dbCloseArea())
		EndIf
		
		dbUseArea(.T.,"TOPCONN",TCGenQry(,,cQuery),'TRB',.F.,.F.)
		
		nReg := Contar("TRB","!EOF()")
		
		If nReg > 0
		
			TRB->(dbCloseArea())
			Alert("Cadastro já existente na base.")
			Return
					
		EndIf	
		
		TRB->(dbCloseArea())
	EndIf
	
	// Caso não seja Pessoa Jurídica		
	If AllTrim(cCod) == ''
		
		cQuery := "SELECT MAX(" + cCpCod + ") COD FROM " + RetSqlName(cTab) + " "+cTab
		cQuery += " WHERE " + cCpFil + " = '" + xFilial(cTab) + "'"
		cQuery += " AND SUBSTRING(" + cCpCod + ",1,1) IN ('0','1','2','3','4','5','6','7','8','9')"
		
		If Select('TRB') <> 0
			dbSelectArea('TRB')
			TRB->(dbCloseArea())
		EndIf
		
		dbUseArea(.T.,"TOPCONN",TCGenQry(,,cQuery),'TRB',.F.,.F.)
		
		dbSelectArea('TRB')
		
		cCod  := Soma1(StrZero(Val(TRB->COD),6))
		cLoja := IIF(cTipo == 'J', Substr(cDoc,9,4), '0001') 
		
		TRB->(dbCloseArea())
	
	Endif
	
	RestArea(aArea)
	
	cNCpCod  := "M->"+Substr(cTab,2,2)+"_COD"
	cNCpLoja := "M->"+Substr(cTab,2,2)+"_LOJA"
	
	&(cNCpCod)  := cCod
	&(cNCpLoja) := cLoja
		
Return cCod
