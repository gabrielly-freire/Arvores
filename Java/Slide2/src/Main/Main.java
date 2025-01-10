package Main;

import java.util.Scanner;
import Entidades.*;
import Aplicacoes.*;

public class Main {

    public static void main(String[] args) {
        Scanner scanner = new Scanner(System.in);
        Arvore<String> arvore = new Arvore<>();
        Metodos<String> metodos = new Metodos<>(arvore);

        while (true) {
            System.out.println("\nEscolha uma opcao:");
            System.out.println("1 - Inserir elemento na arvore");
            System.out.println("2 - Buscar elemento (recursivo)");
            System.out.println("3 - Buscar elemento (iterativo)");
            System.out.println("4 - Remover elemento");
            System.out.println("5 - Mostrar arvore");
            System.out.println("6 - Carregar elementos: (100, 50, 200, 30, 70, 150, 250, 60, 80, 220, 300)");
            System.out.println("7 - Sair");

            int opcao = scanner.nextInt();
            scanner.nextLine();

            switch (opcao) {
                case 1:
                    System.out.println("Digite o ID do elemento:");
                    int idInserir = scanner.nextInt();
                    scanner.nextLine();

                    System.out.println("Digite os dados do elemento:");
                    String dados = scanner.nextLine();

                    arvore.setRaiz(metodos.inserirNo(arvore.getRaiz(), idInserir, dados));
                    System.out.println("Elemento inserido com sucesso!");
                    break;

                case 2:
                    System.out.println("Digite o ID para busca recursiva:");
                    int idBuscarRec = scanner.nextInt();

                    No<String> noEncontradoRec = metodos.buscaRec(arvore.getRaiz(), idBuscarRec);
                    if (noEncontradoRec != null) {
                        System.out.println("Elemento encontrado: " + noEncontradoRec.getDados());
                    } else {
                        System.out.println("Elemento nao encontrado.");
                    }
                    break;

                case 3:
                    System.out.println("Digite o ID para busca iterativa:");
                    int idBuscarIte = scanner.nextInt();

                    No<String> noEncontradoIte = metodos.buscaIte(arvore.getRaiz(), idBuscarIte);
                    if (noEncontradoIte != null) {
                        System.out.println("Elemento encontrado: " + noEncontradoIte.getDados());
                    } else {
                        System.out.println("Elemento nao encontrado.");
                    }
                    break;

                case 4:
                    System.out.println("Digite o ID do elemento a ser removido:");
                    int idRemover = scanner.nextInt();

                    arvore.setRaiz(metodos.removerNo(arvore.getRaiz(), idRemover));
                    System.out.println("Elemento removido (se existia). Verifique a estrutura da arvore se necessario.");
                    break;

                case 5:
                    System.out.println("Arvore em ordem (simetrica):");
                    mostrarArvore(arvore.getRaiz());
                    break;
                    	
                case 6:
                    arvore.setRaiz(metodos.inserirNo(arvore.getRaiz(), 100, "A"));
                    arvore.setRaiz(metodos.inserirNo(arvore.getRaiz(), 50, "B"));
                    arvore.setRaiz(metodos.inserirNo(arvore.getRaiz(), 200, "C"));
                    arvore.setRaiz(metodos.inserirNo(arvore.getRaiz(), 30, "D"));
                    arvore.setRaiz(metodos.inserirNo(arvore.getRaiz(), 70, "E"));
                    arvore.setRaiz(metodos.inserirNo(arvore.getRaiz(), 150, "F"));
                    arvore.setRaiz(metodos.inserirNo(arvore.getRaiz(), 250, "G"));
                    arvore.setRaiz(metodos.inserirNo(arvore.getRaiz(), 60, "H"));
                    arvore.setRaiz(metodos.inserirNo(arvore.getRaiz(), 80, "I"));
                    arvore.setRaiz(metodos.inserirNo(arvore.getRaiz(), 220, "J"));
                    arvore.setRaiz(metodos.inserirNo(arvore.getRaiz(), 300, "K"));

                    System.out.println("Elementos (100, 50, 200, 30, 70, 150, 250, 60, 80, 220, 300) carregados com sucesso!");
                    break;


                case 7:
                    System.out.println("Saindo...");
                    scanner.close();
                    return;

                default:
                    System.out.println("Opcao invalida. Tente novamente.");
            }
        }
    }

    private static void mostrarArvore(No<?> no) {
        if (no != null) {
            mostrarArvore(no.getEsq());
            System.out.println("ID: " + no.getId() + " | Dados: " + no.getDados());
            mostrarArvore(no.getDir());
        }
    }
}
