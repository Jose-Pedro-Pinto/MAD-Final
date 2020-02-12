No prolog
TrabalhoMad é a resolução do problema 1
TrabalhoMad2 é a resolução do problema 2
1º Problema
Prolog
Compilar o programa com:
    compile("Path").  //Path é o caminho relativo ou absoluto
    testDB("DataBase_Path")  //DataBase_Path" é o caminho relativo ou absoluto para o projeto a analizar
1ª pergunta
    proj(Concl,Datas).  //Concl é a data de conclusão mais proxima do projeto
   		   //Datas é uma lista com os dominios de inicio de cada tarefa

    para obter um valor especifico para as datas
    proj(Concl,Datas),labeling(Datas).
2ª pergunta
    workersForES(Trabs).  //Numero de trabalhadores se as tarefas começarem no Earliest Start
3ª pergunta
    criticalWorkers(Trabs).  //Numero de trabalhadores par completar as tarefas criticas
4ª pergunta
    minimumWorkers(Solution).  //retorna valores do tipo
			    //sol(NumTrabs,Datas)
			    //em que NumTrabs é o numero minimo de trabalhadores
			    //e Datas são as datas de inicio
Java
Compilar o programa com:
    Javac "Path"  //Path é o caminho relativo ou absoluto
Correm com
    Java "outPath" "DataBase_Path"  //outPath é o caminho relativo ou absoluto da compilação
			          //DataBase_Path" é o caminho relativo ou absoluto para o projeto a analizar
2º Problema
Compilar o programa com:
    compile("Path").  //Path é o caminho relativo ou absoluto
    testDB("DataBase_Path")  //DataBase_Path" é o caminho relativo ou absoluto para o projeto a analizar
    proj().