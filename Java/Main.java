import static parser.Parser.*;

import daa_structures.Grafo;

import java.util.*;

public class Main {
  public static void main(String[] args){

    if (args.length != 1){
      System.out.println("Precisa de um ficheiro de base de dados de argumento, e só apenas um");
      return;
    }

    Tarefas tarefas;
    String file_path = args[0];
    try {
      //Buscar as tarefas da base de dados
      tarefas = parseFile(file_path);
    }catch (Exception e){
      System.out.println("Ficheiro não existe");
      return;
    }

    //Converter as precedencias de tarefas num grafo para simplificar a implementação

    int[] numero_trabalhadores = new int[tarefas.n_tarefas+2];
    int[] duracoes = new int[tarefas.n_tarefas+2];
    Grafo grafoTarefas = new Grafo(tarefas.n_tarefas+1);
    for(Tarefa tarefa : tarefas.tarefas){
      if (tarefa.filhos.size()!=0) {
        for (int filho : tarefa.filhos) {
          grafoTarefas.insert_new_arc(tarefa.id, filho, tarefa.duracao);
          duracoes[tarefa.id]=tarefa.duracao;
        }
      }else {
        grafoTarefas.insert_new_arc(tarefa.id, tarefas.n_tarefas+1, tarefa.duracao);
        duracoes[tarefa.id]=tarefa.duracao;
        duracoes[tarefas.n_tarefas+1]=0;
      }
      numero_trabalhadores[tarefa.id] = tarefa.numero_trabalhadores;
    }


    //Problema1 - primeira pergunta
    System.out.println("\n\nProblema 1 - Primeira Pergunta");
    long p1p1StartTime = System.currentTimeMillis();
    ESEFParents ESEFParentsData = calcularESEF(grafoTarefas,duracoes);
    int duracao_minima_sem_restricoes= ESEFParentsData.inicio[tarefas.n_tarefas+1];
    System.out.println("Duração minima sem restrições: "+duracao_minima_sem_restricoes);
    long p1p1EndTime = System.currentTimeMillis();
    System.out.println("Duracao: "+(p1p1EndTime-p1p1StartTime)+"ms");


    //Problema1 - segunda pergunta
    System.out.println("\n\nProblema 1 - Segunda Pergunta");
    long p1p2StartTime = System.currentTimeMillis();
    //Multimap<Integer, Acao> actions_sorted = ArrayListMultimap.create();
    Map<Integer,Collection<Acao>> actions_sorted = new HashMap<>();
    for(int i = 1; i< ESEFParentsData.inicio.length; i++) {

      putOnMap(actions_sorted,ESEFParentsData.inicio[i],new Acao(false,i));
    }
    for(int i = 1; i< ESEFParentsData.fim.length; i++){
      putOnMap(actions_sorted,ESEFParentsData.fim[i],new Acao(true,i));
    }
    int numero_trabalhadores_se_as_tarefas_comecarem_no_ES = numeroTrabalhadoresSeAsTarefasComecaremNoES(actions_sorted,numero_trabalhadores);
    System.out.println("Numero de trabalhadores necessarios se as tarefas começarem na data de inicio mais proxima: "+numero_trabalhadores_se_as_tarefas_comecarem_no_ES);
    long p1p2EndTime = System.currentTimeMillis();
    System.out.println("Duracao: "+(p1p2EndTime-p1p2StartTime)+"ms");



    //Problema1 - terceira pergunta
    //Tarefas criticas são tarefas que tem o ES = LS, isto é não podem ser "shifted" na linha temporal
    System.out.println("\n\nProblema 1 - Terceira Pergunta");
    long p1p3StartTime = System.currentTimeMillis();
    Grafo grafoTarefasTransposto = grafoTarefas.geraTransposto();
    int [] LS = calcularLS(grafoTarefas,grafoTarefasTransposto,ESEFParentsData.inicio);


    Map<Integer,Collection<Acao>> atividades_criticas = new HashMap<>();
    for(int i=1;i<ESEFParentsData.inicio.length;i++){
      if (ESEFParentsData.inicio[i]==LS[i]){
        putOnMap(atividades_criticas,ESEFParentsData.inicio[i],new Acao(false,i));
      }
    }

    for(int i=1;i<ESEFParentsData.fim.length;i++){
      if (ESEFParentsData.inicio[i]==LS[i]){
        putOnMap(atividades_criticas,ESEFParentsData.inicio[i]+duracoes[i],new Acao(true,i));
      }
    }

    int numero_minimo_trabalhadores_tarefas_criticas=numeroTrabalhadoresSeAsTarefasComecaremNoES(atividades_criticas,numero_trabalhadores);



    System.out.println("Numero de minimo trabalhadores necessarios para as tarefas criticas: "+numero_minimo_trabalhadores_tarefas_criticas);
    long p1p3EndTime = System.currentTimeMillis();
    System.out.println("Duracao: "+(p1p3EndTime-p1p3StartTime)+"ms");


    //Problema1 - quarta pergunta
    System.out.println("\n\nProblema 1 - Quarta Pergunta");
    long p1p4StartTime = System.currentTimeMillis();

    Resultado resultado = calcularSolucaoProblema(grafoTarefas,actions_sorted,ESEFParentsData.inicio,ESEFParentsData.fim,LS,numero_trabalhadores,new LinkedList<>(),duracoes);
    System.out.println("Numero minimo de trabalhadores necessários para completar a tarefa o mais cedo possível: "+resultado.numero_minimo_necessario_trabalhadores);
    System.out.println("Datas inicio:");
    System.out.println("Tarefa \t-> Inicio");
    for(int i=1;i<resultado.tempos_inicio_tarefas.length-1;i++){
      System.out.println(i+" \t\t-> "+resultado.tempos_inicio_tarefas[i]);
    }
    if (resultado.solucoes_alternativas) System.out.println("Existem soluções alternativas");
    else System.out.println("Não existem soluções alternativas");
    long p1p4EndTime = System.currentTimeMillis();
    System.out.println("Duracao: "+(p1p4EndTime-p1p4StartTime)+"ms");

  }






  static void putOnMap(Map<Integer,Collection<Acao>> map,int key,Acao acao){
    if (map.get(key)==null){
      LinkedList<Acao> value = new LinkedList<>();
      value.add(acao);
      map.put(key,value);
    }else{
      LinkedList<Acao> value = (LinkedList<Acao>) map.get(key);
      value.add(acao);
    }
  }



  static class ESEFParents {
    int[] inicio;
    int[] prec;
    int[] fim;
    ESEFParents(int[] inicio, int[] prec, int[] fim){
      this.inicio = inicio;
      this.prec = prec;
      this.fim = fim;
    }
  }

  private static ESEFParents calcularESEF(Grafo tarefas, int[] duracoes) {
    Stack<Integer> stack = new Stack<>();
    int[] grauE = new int[tarefas.num_vertices()+1];
    for(int i=1;i<grauE.length;i++){
      grauE[i]=0;
    }
    for(int i=1;i<grauE.length;i++){
      for(Grafo.Arco a:tarefas.adjs_no(i)){
        grauE[a.no_final]++;
      }
    }
    for(int i=1;i<grauE.length;i++){
      if (grauE[i]==0){
        stack.push(i);
      }
    }

    int[] inicio = new int[tarefas.num_vertices()+1];
    int[] fim = new int[tarefas.num_vertices()+1];
    int[] prec = new int[tarefas.num_vertices()+1];
    for(int i=1;i<prec.length;i++){
      prec[i]=-1;
    }
    for(int i=1;i<inicio.length;i++){
        inicio[i]=0;
    }

    while(!stack.empty()){
      int no = stack.pop();
      if (prec[no]!=-1) fim[no] = fim[prec[no]] + duracoes[no];
      else fim[no] = duracoes[no];
      for(Grafo.Arco arco : tarefas.adjs_no(no)){
        if (inicio[no]+arco.valor > inicio[arco.no_final]){
          inicio[arco.no_final]=inicio[no]+arco.valor;
          prec[arco.no_final]=no;
        }
        grauE[arco.no_final]--;
        if (grauE[arco.no_final]==0){
          stack.push(arco.no_final);
        }
      }
    }

    return new ESEFParents(inicio,prec,fim);
  }

  static class Acao {
    boolean fim_tarefa;
    int tarefa;
    Acao(boolean fim_tarefa, int tarefa){
      this.fim_tarefa = fim_tarefa;
      this.tarefa = tarefa;
    }
  }

  private static int numeroTrabalhadoresSeAsTarefasComecaremNoES(Map<Integer,Collection<Acao>> actions_sorted, int[] numero_trabalhadores) {


    int prevTrabs =0;
    int max_numero_trabalhadores =0;
    SortedSet<Integer> sortedTimes = new TreeSet<>(actions_sorted.keySet());
    for(int times : sortedTimes){

      Collection<Acao> tarefas_nesse_timestamp = actions_sorted.get(times);
      int balanco_trabalhadores=0;
      for(Acao acao : tarefas_nesse_timestamp){
        int tarefa = acao.tarefa;
        int dispensados=0;
        int requested = 0;
        if (acao.fim_tarefa){
          dispensados += numero_trabalhadores[tarefa];
        }else{
          requested += numero_trabalhadores[tarefa];
        }

        balanco_trabalhadores += (requested-dispensados);
      }
      max_numero_trabalhadores = Math.max(max_numero_trabalhadores,prevTrabs+balanco_trabalhadores);
      prevTrabs+=balanco_trabalhadores;

    }

    return max_numero_trabalhadores;
  }


  private static int [] calcularLS(Grafo grafoTarefas,Grafo grafoTarefasTransposto,int[] inicio){
    int DurMin = inicio[inicio.length-1];
    int[] LS = new int[inicio.length];
    for(int i=1;i<LS.length;i++){
      LS[i]=Integer.MAX_VALUE;
    }
    LS[inicio.length-1]=DurMin;
    Stack<Integer> stack = new Stack<>();
    int[] GrauS=new int[inicio.length];

    for(int i=1;i<GrauS.length;i++){
      for(Grafo.Arco a : grafoTarefas.adjs_no(i)){
        GrauS[i]++;
      }
    }
    for(int i=1;i<GrauS.length;i++){
      if (GrauS[i]==0){
        stack.push(i);
      }
    }

    while(!stack.isEmpty()){
      int v = stack.pop();
      for(Grafo.Arco a: grafoTarefasTransposto.adjs_no(v)){
        if (LS[a.no_final] > LS[v]-a.valor){
          LS[a.no_final]=LS[v]-a.valor;
        }
        GrauS[a.no_final]--;
        if (GrauS[a.no_final]==0){
          stack.push(a.no_final);
        }
      }
    }
    return LS;
  }


  private static class Resultado{
    boolean solucoes_alternativas;
    int[] tempos_inicio_tarefas;
    int numero_minimo_necessario_trabalhadores;

    Resultado(boolean solucoes_alternativas,int[] tempos_inicio_tarefas,int numero_minimo_necessario_trabalhadores){
      this.solucoes_alternativas=solucoes_alternativas;
      this.tempos_inicio_tarefas = tempos_inicio_tarefas;
      this.numero_minimo_necessario_trabalhadores = numero_minimo_necessario_trabalhadores;

    }
  }



  private static Resultado calcularSolucaoProblema(Grafo tarefas,Map<Integer,Collection<Acao>> actions_sorted,int[] ES,int[] EF,int[]LS,int[] numero_trabalhadores,LinkedList<Integer> tarefas_usadas,int[] duracoes) {
    //Construir um DFS em que em cada nó temos um diagrama temporal , a solucao nesse ponto é a min(solucao_filhos,min_atual)

    int [] best_inicios =null;
    boolean alternativas=false;
    int minimo_trabalhadores_necessarios=Integer.MAX_VALUE;

    boolean existem_alteracoes=false;
    for(int time : actions_sorted.keySet()){
        Collection<Acao> acoes_no_timestamp = actions_sorted.get(time);
        for (Acao acao : acoes_no_timestamp) {
          if (!acao.fim_tarefa) {
            if (LS[acao.tarefa] != time && !tarefas_usadas.contains(acao.tarefa)) {
              //Tarefa não critica
              for (int shift = 0; shift <= LS[acao.tarefa] - time; shift++) {
                existem_alteracoes = true;

                LinkedList<Integer> filhos_tarefa = new LinkedList<>();
                for (Grafo.Arco a : tarefas.adjs_no(acao.tarefa)) {
                  filhos_tarefa.addLast(a.no_final);
                }

                Map<Integer, Collection<Acao>> newActions = new HashMap<>();
                int[] new_ES = new int[ES.length];
                int[] new_EF = new int[EF.length];

                for (int i = 1; i < ES.length; i++) {
                  if (i == acao.tarefa) {
                      putOnMap(newActions,ES[i] + shift, new Acao(false, i));
                      new_ES[i] = ES[i] + shift;
                  }else if (filhos_tarefa.contains(i)){

                    int newShift=EF[acao.tarefa]+shift;
                    int greatestEF = 0;

                    for(int j=1;j<=tarefas.num_vertices();j++){
                      for(Grafo.Arco a : tarefas.adjs_no(j)){
                        if (a.no_final==i){
                          if (EF[j]>greatestEF){
                            greatestEF = EF[j];
                          }
                        }
                      }
                    }
                    if (newShift<greatestEF){
                      newShift=greatestEF;
                    }

                    putOnMap(newActions,newShift, new Acao(false, i));
                    new_ES[i] = newShift;

                  }else {
                    putOnMap(newActions,ES[i], new Acao(false, i));
                    new_ES[i] = ES[i];
                  }
                }
                for (int i = 1; i < EF.length; i++) {
                      putOnMap(newActions,new_ES[i]+duracoes[i], new Acao(true, i));
                      new_EF[i] = new_ES[i]+duracoes[i];
                }

                  LinkedList<Integer> new_tar = new LinkedList<>(tarefas_usadas);
                  new_tar.addLast(acao.tarefa);
                  Resultado resultado = calcularSolucaoProblema(tarefas, newActions, new_ES, new_EF, LS, numero_trabalhadores, new_tar, duracoes);
                  if (minimo_trabalhadores_necessarios > resultado.numero_minimo_necessario_trabalhadores) {
                    minimo_trabalhadores_necessarios = resultado.numero_minimo_necessario_trabalhadores;
                    best_inicios = resultado.tempos_inicio_tarefas;
                    alternativas = false;
                  } else if (minimo_trabalhadores_necessarios == resultado.numero_minimo_necessario_trabalhadores) {
                    alternativas = true;
                    //best_inicios = resultado.tempos_inicio_tarefas;
                  }

              }
            }
          }
     }
    }

    if(!existem_alteracoes){
      minimo_trabalhadores_necessarios = numeroTrabalhadoresSeAsTarefasComecaremNoES(actions_sorted,numero_trabalhadores);
      best_inicios=ES;
      alternativas=false;
    }


    return new Resultado(alternativas,best_inicios,minimo_trabalhadores_necessarios);
  }
}
