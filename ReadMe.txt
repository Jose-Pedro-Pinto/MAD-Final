No prolog
TrabalhoMad � a resolu��o do problema 1
TrabalhoMad2 � a resolu��o do problema 2
1� Problema
Prolog
Compilar o programa com:
    compile("Path").  //Path � o caminho relativo ou absoluto
    testDB("DataBase_Path")  //DataBase_Path" � o caminho relativo ou absoluto para o projeto a analizar
1� pergunta
    proj(Concl,Datas).  //Concl � a data de conclus�o mais proxima do projeto
   		   //Datas � uma lista com os dominios de inicio de cada tarefa

    para obter um valor especifico para as datas
    proj(Concl,Datas),labeling(Datas).
2� pergunta
    workersForES(Trabs).  //Numero de trabalhadores se as tarefas come�arem no Earliest Start
3� pergunta
    criticalWorkers(Trabs).  //Numero de trabalhadores par completar as tarefas criticas
4� pergunta
    minimumWorkers(Solution).  //retorna valores do tipo
			    //sol(NumTrabs,Datas)
			    //em que NumTrabs � o numero minimo de trabalhadores
			    //e Datas s�o as datas de inicio
Java
Compilar o programa com:
    Javac "Path"  //Path � o caminho relativo ou absoluto
Correm com
    Java "outPath" "DataBase_Path"  //outPath � o caminho relativo ou absoluto da compila��o
			          //DataBase_Path" � o caminho relativo ou absoluto para o projeto a analizar
2� Problema
Compilar o programa com:
    compile("Path").  //Path � o caminho relativo ou absoluto
    testDB("DataBase_Path")  //DataBase_Path" � o caminho relativo ou absoluto para o projeto a analizar
    proj().