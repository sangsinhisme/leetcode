import BinaryTree.*
@main
def main(): Unit = {

  val data = "[1,2,3,4,5,6,7]"
  val root = TreeNode().stringToTreeNode(data)
  val to_delete = List(3, 5).toArray
  for (i <- delNodes(root, to_delete)){
    TreeNode().printTree(i)
  }
}


