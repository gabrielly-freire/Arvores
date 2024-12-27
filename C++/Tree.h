//
// Created by vitor on 09/12/24.
//

#ifndef TREE_H
#define TREE_H

struct TreeNode {
    int value;
    TreeNode* leftSon;
    TreeNode* rightSon;

    explicit TreeNode(int value);
};

void preOrder(TreeNode* root);
void inOrder(TreeNode* root);
void postOrder(TreeNode* root);
void levelOrder(TreeNode* root);
void printTree(TreeNode* root);

#endif //TREE_H
