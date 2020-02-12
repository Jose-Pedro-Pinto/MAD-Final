package parser;

import java.io.File;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.Scanner;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class Parser {
  public static class Tarefa{
    public int id;
    public LinkedList<Integer> filhos;
    public int duracao;
    public int numero_trabalhadores;


    Tarefa(int id,LinkedList<Integer> filhos,int duracao,int numero_trabalhadores){
      this.id = id;
      this.filhos = new LinkedList<>(filhos);
      this.duracao = duracao;
      this.numero_trabalhadores = numero_trabalhadores;
    }
  }

  public static class Tarefas{
    public LinkedList<Tarefa> tarefas;
    public int n_tarefas;

    Tarefas(LinkedList<Tarefa> tarefas,int n_tarefas){
      this.tarefas = tarefas;
      this.n_tarefas = n_tarefas;
    }
  }

  static Set<Integer> set_tarefas = new HashSet<>();



  public static Tarefas parseFile(String file_path) throws Exception{

    LinkedList<Tarefa> tarefas = new LinkedList<>();
    File database = new File(file_path);
    Scanner scan = new Scanner(database);

    while (scan.hasNextLine()) {
      String toParse = scan.nextLine();
      if (toParse.length()!=0){
        tarefas.add(parseTarefa(toParse));
      }

    }
    scan.close();

    return new Tarefas(tarefas,set_tarefas.size());
  }

  static Tarefa parseTarefa(String tarefa_string){
    tarefa_string = tarefa_string.substring("tarefa(".length(),tarefa_string.length()-").".length());
    String pattern = "([^,]*),\\[(.*)\\],([^,]*),(.*)";
    Pattern p = Pattern.compile(pattern);
    Matcher m = p.matcher(tarefa_string);
    if (m.find()){
      int id = Integer.parseInt(m.group(1));
      int duracao = Integer.parseInt(m.group(3));
      int numero_trabalhadores = Integer.parseInt(m.group(4));
      String filhos_string = m.group(2);
      LinkedList<Integer> filhos = new LinkedList<>();
      if (!filhos_string.equals("")){
        String[] filhos_array = filhos_string.split(",");
        for (String f : filhos_array) {
          int f_id = Integer.parseInt(f);
          filhos.add(f_id);
          set_tarefas.add(f_id);
        }
      }
      set_tarefas.add(id);
      return new Tarefa(id,filhos,duracao,numero_trabalhadores);
    }

    return null;
  }
}
