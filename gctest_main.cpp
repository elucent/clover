#include "lib/gc.h"
#include "lib/io.h"

// struct List {
//     List* next;
//     i32 data[6];
//     List(List* l): next(l) {}
// };

// i32 main(int argc, char** argv) {
//     List* ptr = nullptr;
//     while (true) ptr = new List(ptr);
// }

struct Node {
    Node *root, *left, *right;
};

Node* createTree(Node* root, i32 depth) {
    Node* node = gc_alloc<Node>();
    if (!root) root = node;

    if (depth > 0) {
        depth --;
        node->root = root;
        node->left = createTree(root, depth);
        node->right = createTree(root, depth);
    }
    else {
        node->left = node->right = nullptr;
    }
    return node;
}

i32 checkTree(Node* node) {
    if (!node->left)
        return 1;
    return checkTree(node->left) + checkTree(node->right) + 1;
}

void loops(i32 iterations, i32 depth) {
    i32 check = 0;
    i32 item = 0;

    do {
        check += checkTree(createTree(nullptr, depth));
        item ++;
    } while (item < iterations);
    println(iterations, "\t trees of depth ", depth, "\t check: ", check);
}

void trees(i32 maxDepth) {
    Node* longLastingNode = createTree(nullptr, maxDepth);
    i32 depth = 4;
    do {
        int iterations = 16 << (maxDepth - depth);
        loops(iterations, depth);
        depth += 2;
    } while (depth <= maxDepth);
    println("long lived tree of depth ", maxDepth, "\t check: ", checkTree(longLastingNode));
}

i32 main(i32 argc, char** argv) {
    i32 maxDepth = 21;
    {
        int stretchDepth = maxDepth + 1;
        println("stretch tree of depth ", stretchDepth, "\t check: ", checkTree(createTree(nullptr, stretchDepth)));
    }
    trees(maxDepth);
    return 0;
}

// int main(int argc, char** argv) {
//     iptr sizes[] = {
//         8, 8, 16, 16, 16, 16, 24, 24, 32, 32, 48, 48, 64, 64, 96, 128,
//         8, 8, 16, 16, 16, 16, 24, 24, 32, 32, 48, 48, 64, 64, 256, 512
//     };

//     void** foos = (void**)gc_alloc_untyped(sizeof(iptr) * 16777216);
//     //void* foos[524288];
//     i64 i = 0;
//     while (true) {
//         foos[i % 16777216] = gc_alloc_untyped(sizes[i % 32]);
//         i ++;
//         if (i == 838860800) break;
//     }
//     return 0;
// }