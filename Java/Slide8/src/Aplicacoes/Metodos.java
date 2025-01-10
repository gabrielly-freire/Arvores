package Aplicacoes;

import Entidades.Arvore;
import Entidades.No;

public class Metodos<T> {

    // BUSCA
    public No<T> buscaRec(Arvore<T> arv, No<T> atual, int id) {
        if (atual == arv.getNIL() || atual.getId() == id) {
            return atual;
        }
        if (id < atual.getId()) {
            return buscaRec(arv, atual.getEsq(), id);
        } else {
            return buscaRec(arv, atual.getDir(), id);
        }
    }

    public No<T> buscar(Arvore<T> arv, int id) {
        return buscaRec(arv, arv.getRaiz(), id);
    }

    // ROTAÇÕES
    private void rotacaoEsquerda(Arvore<T> arv, No<T> x) {
        No<T> y = x.getDir();
        x.setDir(y.getEsq());
        if (y.getEsq() != arv.getNIL()) {
            y.getEsq().setPai(x);
        }
        y.setPai(x.getPai());
        if (x.getPai() == arv.getNIL()) {
            arv.setRaiz(y);
        } else if (x == x.getPai().getEsq()) {
            x.getPai().setEsq(y);
        } else {
            x.getPai().setDir(y);
        }
        y.setEsq(x);
        x.setPai(y);
    }

    private void rotacaoDireita(Arvore<T> arv, No<T> x) {
        No<T> y = x.getEsq();
        x.setEsq(y.getDir());
        if (y.getDir() != arv.getNIL()) {
            y.getDir().setPai(x);
        }
        y.setPai(x.getPai());
        if (x.getPai() == arv.getNIL()) {
            arv.setRaiz(y);
        } else if (x == x.getPai().getDir()) {
            x.getPai().setDir(y);
        } else {
            x.getPai().setEsq(y);
        }
        y.setDir(x);
        x.setPai(y);
    }

    // INSERÇÃO
    public void inserir(Arvore<T> arv, int id, T dados) {
        No<T> z = new No<>(id, dados);
        No<T> y = arv.getNIL();
        No<T> x = arv.getRaiz();

        while (x != arv.getNIL()) {
            y = x;
            if (z.getId() < x.getId()) {
                x = x.getEsq();
            } else {
                x = x.getDir();
            }
        }
        z.setPai(y);
        if (y == arv.getNIL()) {
            arv.setRaiz(z);
        } else if (z.getId() < y.getId()) {
            y.setEsq(z);
        } else {
            y.setDir(z);
        }

        z.setEsq(arv.getNIL());
        z.setDir(arv.getNIL());
        z.setCor(No.Cor.RED);

        inserirFixUp(arv, z);
    }

    private void inserirFixUp(Arvore<T> arv, No<T> z) {
        while (z.getPai().getCor() == No.Cor.RED) {
            if (z.getPai() == z.getPai().getPai().getEsq()) {
                No<T> y = z.getPai().getPai().getDir();
                if (y.getCor() == No.Cor.RED) {
                    z.getPai().setCor(No.Cor.BLACK);
                    y.setCor(No.Cor.BLACK);
                    z.getPai().getPai().setCor(No.Cor.RED);
                    z = z.getPai().getPai();
                } else {
                    if (z == z.getPai().getDir()) {
                        z = z.getPai();
                        rotacaoEsquerda(arv, z);
                    }
                    z.getPai().setCor(No.Cor.BLACK);
                    z.getPai().getPai().setCor(No.Cor.RED);
                    rotacaoDireita(arv, z.getPai().getPai());
                }
            } else {
                No<T> y = z.getPai().getPai().getEsq();
                if (y.getCor() == No.Cor.RED) {
                    z.getPai().setCor(No.Cor.BLACK);
                    y.setCor(No.Cor.BLACK);
                    z.getPai().getPai().setCor(No.Cor.RED);
                    z = z.getPai().getPai();
                } else {
                    if (z == z.getPai().getEsq()) {
                        z = z.getPai();
                        rotacaoDireita(arv, z);
                    }
                    z.getPai().setCor(No.Cor.BLACK);
                    z.getPai().getPai().setCor(No.Cor.RED);
                    rotacaoEsquerda(arv, z.getPai().getPai());
                }
            }
        }
        arv.getRaiz().setCor(No.Cor.BLACK);
    }

    // REMOÇÃO
    public void remover(Arvore<T> arv, int id) {
        No<T> z = buscar(arv, id);
        if (z == arv.getNIL()) {
            // Não encontrado
            return;
        }

        No<T> y = z;
        No<T> x;
        No.Cor yCorOriginal = y.getCor();

        // Caso 1: Nó com dois filhos
        if (z.getEsq() != arv.getNIL() && z.getDir() != arv.getNIL()) {
            // Acha o sucessor
            y = minimo(arv, z.getDir());
            yCorOriginal = y.getCor();
            x = y.getDir();
            if (y.getPai() == z) {
                x.setPai(y);
            } else {
                transplante(arv, y, y.getDir());
                y.setDir(z.getDir());
                y.getDir().setPai(y);
            }
            transplante(arv, z, y);
            y.setEsq(z.getEsq());
            y.getEsq().setPai(y);
            y.setCor(z.getCor());
        } else {
            // Caso 2: Nó com zero ou um filho
            if (z.getEsq() != arv.getNIL()) {
                x = z.getEsq();
            } else {
                x = z.getDir();
            }
            transplante(arv, z, x);
            if (yCorOriginal == No.Cor.BLACK) {
                removerFixUp(arv, x);
            }
        }
    }

    private No<T> minimo(Arvore<T> arv, No<T> no) {
        while (no.getEsq() != arv.getNIL()) {
            no = no.getEsq();
        }
        return no;
    }

    private void transplante(Arvore<T> arv, No<T> u, No<T> v) {
        if (u.getPai() == arv.getNIL()) {
            arv.setRaiz(v);
        } else if (u == u.getPai().getEsq()) {
            u.getPai().setEsq(v);
        } else {
            u.getPai().setDir(v);
        }
        v.setPai(u.getPai());
    }

    private void removerFixUp(Arvore<T> arv, No<T> x) {
        while (x != arv.getRaiz() && x.getCor() == No.Cor.BLACK) {
            if (x == x.getPai().getEsq()) {
                No<T> w = x.getPai().getDir();
                if (w.getCor() == No.Cor.RED) {
                    w.setCor(No.Cor.BLACK);
                    x.getPai().setCor(No.Cor.RED);
                    rotacaoEsquerda(arv, x.getPai());
                    w = x.getPai().getDir();
                }
                if (w.getEsq().getCor() == No.Cor.BLACK && w.getDir().getCor() == No.Cor.BLACK) {
                    w.setCor(No.Cor.RED);
                    x = x.getPai();
                } else {
                    if (w.getDir().getCor() == No.Cor.BLACK) {
                        w.getEsq().setCor(No.Cor.BLACK);
                        w.setCor(No.Cor.RED);
                        rotacaoDireita(arv, w);
                        w = x.getPai().getDir();
                    }
                    w.setCor(x.getPai().getCor());
                    x.getPai().setCor(No.Cor.BLACK);
                    w.getDir().setCor(No.Cor.BLACK);
                    rotacaoEsquerda(arv, x.getPai());
                    x = arv.getRaiz();
                }
            } else {
                No<T> w = x.getPai().getEsq();
                if (w.getCor() == No.Cor.RED) {
                    w.setCor(No.Cor.BLACK);
                    x.getPai().setCor(No.Cor.RED);
                    rotacaoDireita(arv, x.getPai());
                    w = x.getPai().getEsq();
                }
                if (w.getDir().getCor() == No.Cor.BLACK && w.getEsq().getCor() == No.Cor.BLACK) {
                    w.setCor(No.Cor.RED);
                    x = x.getPai();
                } else {
                    if (w.getEsq().getCor() == No.Cor.BLACK) {
                        w.getDir().setCor(No.Cor.BLACK);
                        w.setCor(No.Cor.RED);
                        rotacaoEsquerda(arv, w);
                        w = x.getPai().getEsq();
                    }
                    w.setCor(x.getPai().getCor());
                    x.getPai().setCor(No.Cor.BLACK);
                    w.getEsq().setCor(No.Cor.BLACK);
                    rotacaoDireita(arv, x.getPai());
                    x = arv.getRaiz();
                }
            }
        }
        x.setCor(No.Cor.BLACK);
    }
}
