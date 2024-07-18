import BinaryTree.*
@main
def main(): Unit = {

  val data = "[1,2,3,4,5,6,7]"
  val root = TreeNode().stringToTreeNode(data)
  val allPathLeaf = findDirectionAllNode(root)
  val distance = 3
  var output = 0
  for (i <- allPathLeaf.indices){
    for (j <- i + 1 until allPathLeaf.length)
      val path = allPathLeaf(i)._2
      val path2 = allPathLeaf(j)._2
      if (path.length + path2.length - (firstMatching(path, path2).length * 2) < distance)
        output += 1
  }
  println(output)
}


