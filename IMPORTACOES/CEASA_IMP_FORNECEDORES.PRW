/*
+----------------------------------------------------------------------------+
!                        FICHA TECNICA DO PROGRAMA                           !
+----------------------------------------------------------------------------+
! DADOS DO PROGRAMA 														 !
+------------------+---------------------------------------------------------+
!Tipo 			   ! Atualização 											 !
+------------------+---------------------------------------------------------+
!Modulo 		   ! Cadastros	 											 !
+------------------+---------------------------------------------------------+
!Nome 			   ! CEASA_IMP_FORNECEDORES.PRW							 !
+------------------+---------------------------------------------------------+
!Descricao 		   ! Funções Importação de Fornecedores						 !
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
! Programa 	! ImpFornec ! Autor !Gilson Lima 		 ! Data ! 31/08/2015     !
+-----------+-----------+-------+--------------------+------+----------------+
! Descricao ! Leitura do arquivo											 !
!			! 										 						 !
+----------------------------------------------------------------------------*/
User Function ImpFornec()

	Private cArquivo:= ""
	Private cPerg	:= "IMPFORN"
	//Private aPergs	:= {}
	
	// Cria pergunta caso não exista
	SX1->(dbSeek(xFilial("SX1")+cPerg,.T.))
	If SX1->(!Found())
	
		PutSx1(cPerg,"01","Arquivo Importação","","","mv_ch1","C",99,00,00,"G","","DIR","","","mv_par01","","","","","","","","","","","","","","","","",{"Selecione o Arquivo"},{},{},"")
		//AADD(aPergs,{"Arquivo Importação","Arquivo Importação","Arquivo Importação","mv_ch1","C",60,0,0,"G","","MV_PAR01","","","","","","","","","","","","","","","","","","","","","","","","","DIR","","","",""})
		//AjustaSx1(cPerg,aPergs)
	EndIf
	
	// Carrega Perguntas
	Pergunte(cPerg,.T.)
	
	// Chama a função de importação
	If MsgYesNo("Confirma Importação?","Atenção")
		Processa({||ImpFor()})
	EndIf

Return

/*----------+-----------+-------+--------------------+------+----------------+
! Programa 	! ImpFor    ! Autor !Gilson Lima 		 ! Data ! 31/08/2015     !
+-----------+-----------+-------+--------------------+------+----------------+
! Descricao ! Leitura do arquivo											 !
!			! 										 						 !
+----------------------------------------------------------------------------*/
Static Function ImpFor()

	//#DEFINE _POS_CDANT	1	// Cód. Sistema Anterior
	#DEFINE _POS_RAZAO	1	// Razão Social
	#DEFINE _POS_NFANT	2	// Nome Fantasia
	#DEFINE _POS_DOC	10	// CNPJ / CPF
	#DEFINE _POS_CEP	7	// Cep
	#DEFINE _POS_END	3	// Endereço
	#DEFINE _POS_COMPL	16	// Complemento Endereço
	#DEFINE _POS_BAIRRO	4	// Bairro
	//#DEFINE _POS_DDI	9	// DDI
	#DEFINE _POS_DDDFON	11	// DDD Fone
	#DEFINE _POS_FONE	12	// Fone
	#DEFINE _POS_EMAIL	15	// Email
	#DEFINE _POS_INSCRM	14	// Inscr. Municipal
	//#DEFINE _POS_CDIBGE	14	// Cód. Município
	#DEFINE _POS_INSCRE	13	// Inscr. Estadual
	#DEFINE _POS_UF		5	// UF
	#DEFINE _POS_CID	6	// Cidade
	//#DEFINE _POS_0800	18	// Tel. 0800
	//#DEFINE _POS_FAX	19	// Fax

	Local cArquivo 	:= AllTrim(MV_PAR01)
	Local nHdl		:= 0
	Local nTamFile	:= 0
	Local nRestaLer := 0
	Local nFimLinha := 0
	Local nIniLeit	:= 0
	Local cLeitura	:= ''
	Local cString	:= ''
	Local cEol		:= CHR(13)+CHR(10)
	Local lContinua	:= .T.
	
	Local aDados	:= {}
	Local aArquivo	:= {}
	
	Local nImport	:= 0
	
	Private aErros	:= {}
	Private aLogs	:= {}
	
	GrvLog("Produtos - Importação de Arquivo")
	GrvLog("Data e Hora Ínicio.: " + DtoC( Date() )  + " - " + Time() )
	GrvLog(Replicate('-',80))
	
	nHdl    := fOpen(cArquivo,68)
	
	If nHdl == -1
		MsgStop("O arquivo "+cArquivo+" não pode ser aberto... Verifique os parâmetros","Atencao")
	    Return(cArqDbf)
	Endif	
	
	nTamFile := nRestaLer := fSeek(nHdl,0,2)
	fSeek(nHdl,0,0)	
	
	While nRestaLer > 0
		
		aDados := {}
		
		cLeitura  := fReadStr(nHdl,65535)
	    nFimLinha := IIF(AT(cEol,cLeitura)==0, nRestaLer+1, AT(cEol,cLeitura)+1) 
		cString   := Substr(cLeitura,1,nFimLinha)
		If nFimLinha-1 <> nRestaLer
	    	cString   := Substr(cString,1,Len(cString)-2)
	    EndIf
		nRestaLer -= Len(cString)+2
		nIniLeit  += Len(cString)+2 
		
		fSeek(nHdl,nIniLeit)
			    
	    aDados := Separa(cString,'|',.T.)
	    
	    aAdd(aArquivo,aDados)
			
	EndDo
	
	fClose(nHdl)
	
	For nA := 1 To Len(aArquivo)
	
		nImport++
		
		aCod := GeraCod(aArquivo[nA][_POS_DOC])
		
		aArquivo[nA][_POS_RAZAO] := Upper(RetGraf(aArquivo[nA][_POS_RAZAO]))
		aArquivo[nA][_POS_NFANT] := Upper(RetGraf(aArquivo[nA][_POS_NFANT]))

		aArquivo[nA][_POS_DOC]   := StrTran(aArquivo[nA][_POS_DOC],'.','')
		aArquivo[nA][_POS_DOC]   := StrTran(aArquivo[nA][_POS_DOC],',','')
		aArquivo[nA][_POS_DOC]   := StrTran(aArquivo[nA][_POS_DOC],'/','')
		aArquivo[nA][_POS_DOC]   := StrTran(aArquivo[nA][_POS_DOC],'-','')

		aArquivo[nA][_POS_FONE]  := StrTran(aArquivo[nA][_POS_FONE],'-','')
		//aArquivo[nA][_POS_FAX]   := StrTran(aArquivo[nA][_POS_FONE],'-','')

		//aArquivo[nA][_POS_0800]   := StrTran(aArquivo[nA][_POS_DOC],' ','')
		//aArquivo[nA][_POS_0800]   := StrTran(aArquivo[nA][_POS_DOC],'-','')

		aArquivo[nA][_POS_CEP]   := StrTran(aArquivo[nA][_POS_CEP],'-','')
		aArquivo[nA][_POS_END] 	 := Upper(RetGraf(aArquivo[nA][_POS_END]))
		aArquivo[nA][_POS_COMPL] := Upper(RetGraf(aArquivo[nA][_POS_COMPL]))
		aArquivo[nA][_POS_BAIRRO]:= Upper(RetGraf(aArquivo[nA][_POS_BAIRRO]))
		aArquivo[nA][_POS_EMAIL] := Upper(aArquivo[nA][_POS_EMAIL])
		aArquivo[nA][_POS_INSCRM]:= Upper(aArquivo[nA][_POS_INSCRM])

		//aArquivo[nA][_POS_CDIBGE]:= Substr(aArquivo[nA][_POS_CDIBGE],3,5)
		aArquivo[nA][_POS_INSCRE]:= Upper(aArquivo[nA][_POS_INSCRE])

		aArquivo[nA][_POS_UF]	 := Upper(aArquivo[nA][_POS_UF])
		aArquivo[nA][_POS_CID]	 := Upper(RetGraf(aArquivo[nA][_POS_CID]))
		
		If aCod[4]
			
			dbSelectArea('SA2')
			SA2->(dbSetOrder(1))
			If SA2->(dbSeek(xFilial('SA2')+aCod[1]+aCod[2]))
				RecLock('SA2',.F.)
			EndIf
		Else
			RecLock('SA2',.T.)
		EndIf
		
		aEnd := Separa(aArquivo[nA][_POS_END],',')
		
		cEnd := IIF(Len(aEnd) > 0, AllTrim(aEnd[1]), aArquivo[nA][_POS_END])

		cNr  := IIF(Len(aEnd) == 2, AllTrim(aEnd[2]), '')
		
		SA2->A2_FILIAL	:= xFilial('SA2')
		SA2->A2_COD		:= aCod[1]
		SA2->A2_LOJA	:= aCod[2]
		//SA2->A2_XCODANT	:= aArquivo[nA][_POS_CDANT]
		SA2->A2_NOME	:= aArquivo[nA][_POS_RAZAO]
		SA2->A2_NREDUZ	:= aArquivo[nA][_POS_NFANT]
		SA2->A2_END		:= cEnd
		SA2->A2_NR_END	:= cNr
		SA2->A2_ENDCOMP	:= aArquivo[nA][_POS_COMPL]
		SA2->A2_BAIRRO	:= aArquivo[nA][_POS_BAIRRO]
		SA2->A2_EST	    := aArquivo[nA][_POS_UF]
		SA2->A2_CONTPRE := '1'
		SA2->A2_MUN	    := aArquivo[nA][_POS_CID]
		SA2->A2_CEP		:= aArquivo[nA][_POS_CEP]
		SA2->A2_PAIS	:= '105'	// BRASIL (Cod. Bacen)
		SA2->A2_TIPO	:= aCod[3]
		SA2->A2_CGC		:= aArquivo[nA][_POS_DOC]
		//SA2->A2_DDI		:= aArquivo[nA][_POS_DDI]
		SA2->A2_DDD		:= aArquivo[nA][_POS_DDDFON]
		SA2->A2_TEL		:= aArquivo[nA][_POS_FONE]
		//SA2->A2_FAX		:= aArquivo[nA][_POS_FAX]
		//SA2->A2_XTL0800	:= aArquivo[nA][_POS_0800]
		SA2->A2_INSCRM	:= aArquivo[nA][_POS_INSCRM]
		//SA2->A2_COD_MUN	:= aArquivo[nA][_POS_CDIBGE]
		SA2->A2_INSCR	:= aArquivo[nA][_POS_INSCRE]
		SA2->A2_VINCULA := '1'
		SA2->A2_ID_REPR := '2'
		SA2->A2_B2B		:= '2'
		SA2->A2_PLCRRES := 'N'
		SA2->A2_PLFIL	:= 'N'
		SA2->A2_MSBLQL	:= '2'
		SA2->A2_RECPIS	:= '1'
		SA2->A2_RECCOFI := '1'
		SA2->A2_RECCSLL := '1'
		SA2->A2_RETISI	:= '2'
		SA2->A2_INDRUR	:= '0'
		SA2->A2_REGESIM	:= '2'
		SA2->A2_MJURIDI	:= '2'
		SA2->A2_IMPIP	:= '2'
		SA2->A2_TIPCTA	:= '1'
		SA2->A2_MINIRF	:= '2'
		SA2->A2_CONFFIS	:= '0'
		SA2->A2_PAGGFE	:= '2'
		SA2->A2_FORNEMA	:= '2'
		
		SA2->(MsUnLock())
		
		If Select('SA2') <> 0
			dbSelectArea('SA2')
			SA2->(dbCloseArea())
		EndIf
		
	Next nA
	
	GrvLog("Importação Produtos Concluída")
	GrvLog("Data / Hora Conclusão: " + DtoC( Date() )  + " / " + Time() )
	GrvLog("Registros Importados.: " + cValToChar(nImport))
	GrvLog(Replicate('-',80))
	
	GrvErros()
	ExibeLog()

Return .T. 

/*----------+-----------+-------+--------------------+------+----------------+
! Programa 	! GeraCod	! Autor !Gilson Lima 		 ! Data ! 30/07/2015     !
+-----------+-----------+-------+--------------------+------+----------------+
! Descricao ! Funcao auxiliar de geração de código do fornecedor baseado	 !
! 			! no CNPF definindo automaticamente a loja						 !
+----------------------------------------------------------------------------*/
Static Function GeraCod(cDoc)

	Local aArea		:= GetArea()
	Local aCod		:= Array(4) // 1 - Codigo, 2 = Loja, 3 = Tipo, 4 = Altera
	Local cCod		:= ''
	Local cLoja		:= ''
	Local cTipo		:= IIF(Len(cDoc) == 14, 'J', IIF(Len(cDoc) == 11, 'F', 'X'))
	Local cQuery 	:= ''
	Local lAltera	:= .F.
	Local nReg		:= 0
	Local nReg2		:= 0
	
	// Pessoa Jurídica
	If Len(cDoc) == 14
		
		// Verifica se já existe o CNPJ na Base
		cQuery := "SELECT SA2.A2_COD FROM " + RetSqlName('SA2') + " SA2"
		cQuery += " WHERE SA2.D_E_L_E_T_ <> '*'"
		cQuery += " AND SUBSTRING(SA2.A2_CGC,1,8) = '" + SUBSTR(cDoc,1,8) + "'"
		cQuery += " AND SA2.A2_FILIAL = '" + xFilial('SA2') + "'"
		
		If Select('TRB') <> 0
			dbSelectArea('TRB')
			TRB->(dbCloseArea())
		EndIf
		
		dbUseArea(.T.,"TOPCONN",TCGenQry(,,cQuery),'TRB',.F.,.F.)
		
		nReg := Contar("TRB","!EOF()")
		
		If nReg > 0
			
			dbSelectArea('TRB')
			TRB->(dbGoTop())
			
			cCod  := TRB->A2_COD
			cLoja := Substr(cDoc,9,4)
			
			cQuery := "SELECT SA2.A2_COD, SA2.A2_FILIAL FROM " + RetSqlName('SA2') + " SA2"
			cQuery += " WHERE SA2.D_E_L_E_T_ <> '*'"
			cQuery += " AND SA2.A2_FILIAL = '" + xFilial('SA2') + "'"
			cQuery += " AND SA2.A2_COD = '" + cCod + "'"
			cQuery += " AND SA2.A2_LOJA = '" + cLoja + "'"
			
			If Select('TRB2') <> 0
				dbSelectArea('TRB2')
				TRB2->(dbCloseArea())
			EndIf
			
			dbUseArea(.T.,"TOPCONN",TCGenQry(,,cQuery),'TRB2',.F.,.F.)
			
			nReg2 := Contar("TRB2","!EOF()")
			
			If nReg2 > 0
				lAltera := .T.
			EndIf
			
			TRB2->(dbCloseArea())
		
		EndIf
		
		TRB->(dbCloseArea())
	
	Else
	
		cQuery := "SELECT SA2.A2_COD, SA2.A2_LOJA FROM " + RetSqlName('SA2') + " SA2"
		cQuery += " WHERE SA2.D_E_L_E_T_ <> '*'"
		cQuery += " AND SA2.A2_CGC = '" + PadR(AllTrim(cDoc),TamSX3('A2_CGC')[1], ' ') + "'"
		cQuery += " AND SA2.A2_FILIAL = '" + xFilial('SA2') + "'"
		
		If Select('TRB') <> 0
			dbSelectArea('TRB')
			TRB->(dbCloseArea())
		EndIf
		
		dbUseArea(.T.,"TOPCONN",TCGenQry(,,cQuery),'TRB',.F.,.F.)
		
		nReg := Contar("TRB","!EOF()")
		
		If nReg > 0
		
			dbSelectArea('TRB')
			TRB->(dbGoTop())
			
			cCod	:= TRB->A2_COD
			cLoja	:= TRB->A2_LOJA
			lAltera := .T.
			
			TRB->(dbCloseArea())
		
		EndIf	
	EndIf
	
	// Caso não seja Pessoa Jurídica		
	If AllTrim(cCod) == ''
		
		cQuery := "SELECT MAX(SA2.A2_COD) COD FROM " + RetSqlName('SA2') + " SA2"
		cQuery += " WHERE SA2.D_E_L_E_T_ <> '*'"
		cQuery += " AND SA2.A2_FILIAL = '" + xFilial('SA2') + "'"
		cQuery += " AND SUBSTRING(SA2.A2_COD,1,1) IN ('0','1','2','3','4','5','6','7','8','9')"
		
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
	
	aCod[1] := cCod
	aCod[2] := cLoja
	aCod[3] := cTipo
	aCod[4] := lAltera
		
Return aCod

/*----------+-----------+-------+--------------------+------+----------------+
! Programa 	! GrvErros	! Autor !Gilson Lima 		 ! Data ! 17/03/2015     !
+-----------+-----------+-------+--------------------+------+----------------+
! Descricao ! Funcao auxiliar de adição da mensagem de log no array de Logs	 !
! 			! de sucesso e erros da rotina									 !
+----------------------------------------------------------------------------*/
Static Function GrvErros(cLog)

	// Gera o arquivo texto
	Local cArqTxt 	:= 'C:\TEMP\ERRO_IMPFORN.TXT'
	Local nHdl		:= fCreate(cArqTxt)
	
	Local cLin 	 	:= ""
	Local nLinhas	:= 0
	Local nRegistros:= 0
	
	If nHdl == -1
		MsgAlert("O arquivo de nome "+cArqTxt+" não pode ser executado! Verifique os parâmetros.","Atenção!")
		Return
	EndIf

	// Grava arquivos com erros
	For nB := 1 To Len(aErros)

		nLinhas++

		cLin := ''

		If nB == 1

			cLin += "Erros de Importação: " + CRLF
			cLin += Replicate('-',80) + CRLF
		
		EndIf
		
		cLin += cValToChar(nB) + '|' + AllTrim(aErros[nB][1]) + '|' + AllTrim(aErros[nB][2])+ '|' + AllTrim(aErros[nB][3])
		
		cLin += CRLF

		If nB == Len(aErros)
			cLin += Replicate('-',80)		
		EndIF
		
		// Gravação do arquivo. Testa por erros durante a gravação
		If fWrite(nHdl,cLin,Len(cLin)) != Len(cLin)
			If !MsgAlert("Ocorreu um erro na gravação do arquivo. Continua?","Atenção!")
				Exit
			EndIf
		EndIf	
	
	Next nB
	
	// Fecha o arquivo texto
	fClose(nHdl)	

Return
/*----------+-----------+-------+--------------------+------+----------------+
! Programa 	! GrvLog	! Autor !Gilson Lima 		 ! Data ! 17/03/2015     !
+-----------+-----------+-------+--------------------+------+----------------+
! Descricao ! Funcao auxiliar de adição da mensagem de log no array de Logs	 !
! 			! de sucesso e erros da rotina									 !
+----------------------------------------------------------------------------*/
Static Function GrvLog(cLog)

	aAdd(aLogs,cLog)

Return

/*----------+-----------+-------+--------------------+------+----------------+
! Programa 	! ExibeLog	! Autor !Gilson Lima 		 ! Data ! 17/03/2015     !
+-----------+-----------+-------+--------------------+------+----------------+
! Descricao ! Funcao chamada ao final do processamento para exibição do LOG	 !
! 			! de sucesso e erros da rotina									 !
+----------------------------------------------------------------------------*/
Static Function ExibeLog()

	Local cMask     := "Arquivos Texto" + "(*.TXT)|*.txt|"
	Local cTexto	:= ''
	
	Local   cFile     := ""
	Local   cFileLog  := ""
	Local   oDlg      := NIL
	Local   oFont     := NIL
	Local   oMemo     := NIL	
	
	cTexto := LeLog()

	Define Font oFont Name "Courier New" Size 8, 18

	Define MsDialog oDlg Title "IMPORTACAO PRODUTOS" From 3, 0 to 450, 673 Pixel

	@ 5, 5 Get oMemo Var cTexto Memo Size 330, 200 Of oDlg Pixel
	oMemo:bRClicked := { || AllwaysTrue() }
	oMemo:oFont     := oFont

	Define SButton From 209, 309 Type  1 Action oDlg:End() Enable Of oDlg Pixel // Apaga
	Define SButton From 209, 279 Type 13 Action ( cFile := cGetFile( cMask, "" ), If( cFile == "", .T., ;
	MemoWrite( cFile, cTexto ) ) ) Enable Of oDlg Pixel

	Activate MsDialog oDlg Center

Return

/*----------+-----------+-------+--------------------+------+----------------+
! Programa 	! LeLog		! Autor !Gilson Lima 		 ! Data ! 17/03/2015     !
+-----------+-----------+-------+--------------------+------+----------------+
! Descricao ! Funcao que efetua a leitura do array de LOG do sistema e 	 	 !
! 			! retorna em variável para exibição em tela						 !
+----------------------------------------------------------------------------*/
Static Function LeLog()

	Local cRet := ''
	
	For nA := 1 To Len(aLogs)
		cRet += aLogs[nA] + CRLF
	Next nA
		
Return cRet

/*----------+-----------+-------+--------------------+------+----------------+
! Programa 	! RetGraf	! Autor !Gilson Lima 		 ! Data ! 15/10/2014     !
+-----------+-----------+-------+--------------------+------+----------------+
! Descricao ! Função para retirar os Caracteres Especiais do texto			 !
!			!																 !
+----------------------------------------------------------------------------*/
Static Function RetGraf(_sOrig)

   local _sRet := _sOrig

   _sRet = StrTran (_sRet, "á", "a")
   _sRet = StrTran (_sRet, "é", "e")
   _sRet = StrTran (_sRet, "í", "i")
   _sRet = StrTran (_sRet, "ó", "o")
   _sRet = StrTran (_sRet, "ú", "u")
   _sRet = StrTran (_sRet, "Á", "A")
   _sRet = StrTran (_sRet, "É", "E")
   _sRet = StrTran (_sRet, "Í", "I")
   _sRet = StrTran (_sRet, "Ó", "O")
   _sRet = StrTran (_sRet, "Ú", "U")
   _sRet = StrTran (_sRet, "ã", "a")
   _sRet = StrTran (_sRet, "õ", "o")
   _sRet = StrTran (_sRet, "Ã", "A")
   _sRet = StrTran (_sRet, "Õ", "O")
   _sRet = StrTran (_sRet, "â", "a")
   _sRet = StrTran (_sRet, "ê", "e")
   _sRet = StrTran (_sRet, "î", "i")
   _sRet = StrTran (_sRet, "ô", "o")
   _sRet = StrTran (_sRet, "û", "u")
   _sRet = StrTran (_sRet, "Â", "A")
   _sRet = StrTran (_sRet, "Ê", "E")
   _sRet = StrTran (_sRet, "Î", "I")
   _sRet = StrTran (_sRet, "Ô", "O")
   _sRet = StrTran (_sRet, "Û", "U")
   _sRet = StrTran (_sRet, "ç", "c")
   _sRet = StrTran (_sRet, "Ç", "C")
   _sRet = StrTran (_sRet, "à", "a")
   _sRet = StrTran (_sRet, "À", "A")
   _sRet = StrTran (_sRet, "º", ".")
   _sRet = StrTran (_sRet, "ª", ".")
   _sRet = StrTran (_sRet, chr (9), " ") // TAB
   
Return _sRet