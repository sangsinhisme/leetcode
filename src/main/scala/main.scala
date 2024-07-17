import BinaryTree.*
@main
def main(): Unit = {

  val data = "[1,null,10,12,13,4,6,null,15,null,null,5,11,null,2,14,7,null,8,null,null,null,9,3]"
  val root = TreeNode().stringToTreeNode(data)
  val output = findDirection(root, 6)
  println(getDirections(root, 6, 15))
}


