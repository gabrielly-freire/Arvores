package Arvore;

import java.util.LinkedList;
import java.util.Queue;

public class ABB<T> {

    private No<T> raiz;


    public void inserir(int id, T dados) {
        No<T> novoNo = new No<>(id, dados); 

        if (getRaiz() == null) {
            setRaiz(novoNo);
        } else {
            inserirRec(getRaiz(), novoNo);
        }
    }

    private void inserirRec(No<T> atualNo, No<T> novoNo) {
        if (novoNo.getId() < atualNo.getId()) { 
            if (atualNo.getEsq() == null) {
                atualNo.setEsq(novoNo); 
            } else {
                inserirRec(atualNo.getEsq(), novoNo);
            }
        } else { 
            if (atualNo.getDir() == null) {
                atualNo.setDir(novoNo);
            } else {
                inserirRec(atualNo.getDir(), novoNo);
            }
        }
    }


    public void imprimirPreOrdem() {
        imprimirPreOrdemRec(getRaiz());
    }

    private void imprimirPreOrdemRec(No<T> no) {
        if (no != null) {
            System.out.println("ID: " + no.getId() + ", Dados: " + no.getDados());
            imprimirPreOrdemRec(no.getEsq());
            imprimirPreOrdemRec(no.getDir()); 
        }
    }


    public void imprimirOrdemSimetrica(No<T> no) {
        if (no != null) {
            imprimirOrdemSimetrica(no.getEsq());
            System.out.println("ID: " + no.getId() + ", Dados: " + no.getDados());
            imprimirOrdemSimetrica(no.getDir());
        }
    }


    public void imprimirPosOrdem(No<T> no) {
        if (no != null) {
            imprimirPosOrdem(no.getDir()); 
            System.out.println("ID: " + no.getId() + ", Dados: " + no.getDados());
            imprimirPosOrdem(no.getEsq());
        }
    }

    public void imprimirNivel() {
        if (getRaiz() == null) {
            return;
        }

        Queue<No<T>> fila = new LinkedList<>(); 
        fila.add(getRaiz()); 

        while (!fila.isEmpty()) {
            No<T> atual = fila.poll(); 
            System.out.println("ID: " + atual.getId() + ", Dados: " + atual.getDados());

            if (atual.getEsq() != null) {
                fila.add(atual.getEsq());
            }

            if (atual.getDir() != null) {
                fila.add(atual.getDir());
            }
        }
    }


    public No<T> getRaiz() {
        return raiz;
    }


    public void setRaiz(No<T> raiz) {
        this.raiz = raiz;
    }
}
