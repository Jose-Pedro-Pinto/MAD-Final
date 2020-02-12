package daa_structures;

/*-------------------------------------------------------------------*\
|  Definicao de grafos com UM peso (int)                              |
|     Assume-se que os vertices sao numerados de 1 a |V|.             |
|                                                                     |
|   A.P.Tomas, CC2001 (material para prova pratica), DCC-FCUP, 2017   |
|   Last modified: 2017.12.18                                         |
\--------------------------------------------------------------------*/

import java.util.LinkedList;

public class Grafo {

  public class Arco {
    public int no_final;
    public int valor;

    Arco(int fim, int v) {
      no_final = fim;
      valor = v;
    }

    int extremo_final() {
      return no_final;
    }

    int valor_arco() {
      return valor;
    }

    void novo_valor(int v) {
      valor = v;
    }
  }


  public class No {
    //int label;
    LinkedList<Arco> adjs;

    No() {
      adjs = new LinkedList<Arco>();
    }
  }


  No verts[];
  int nvs, narcos;

  public Grafo(int n) {
    nvs = n;
    narcos = 0;
    verts = new No[n + 1];
    for (int i = 0; i <= n; i++)
      verts[i] = new No();
    // para vertices numerados de 1 a n (posicao 0 nao vai ser usada)
  }

  public int num_vertices() {
    return nvs;
  }

  public int num_arcos() {
    return narcos;
  }

  public LinkedList<Arco> adjs_no(int i) {
    return verts[i].adjs;
  }

  public void insert_new_arc(int i, int j, int valor_ij) {
    verts[i].adjs.addFirst(new Arco(j, valor_ij));
    narcos++;
  }

  public Arco find_arc(int i, int j) {
    for (Arco adj : adjs_no(i))
      if (adj.extremo_final() == j) return adj;
    return null;
  }


  public Grafo geraTransposto(){
    Grafo g = new Grafo(nvs);
    for(int i=1;i<=nvs;i++){
      No v = verts[i];
      for(Arco a : v.adjs){
        g.insert_new_arc(a.no_final,i,a.valor);
      }
    }
    return g;
  }
}