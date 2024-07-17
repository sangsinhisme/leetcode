import scala.annotation.tailrec

object BinaryTree {
  /**
   * Definition for a binary tree node.
   * class TreeNode(_value: Int = 0, _left: TreeNode = null, _right: TreeNode = null) {
   * var value: Int = _value
   * var left: TreeNode = _left
   * var right: TreeNode = _right
   * }
   */
    def getDirections(root: TreeNode, startValue: Int, destValue: Int): String = {
      val startPath: String = findDirection(root, startValue)
      val destPath: String = findDirection(root, destValue)
      val firstMatch = firstMatching(destPath, startPath)

      if (firstMatch.nonEmpty) {
        startPath.substring(firstMatch.length).map(_ => "U").mkString("") + destPath.substring(firstMatch.length)
      }

      else startPath.map(_ => "U").mkString("") + destPath
    }

    def delNodes(root: TreeNode, to_delete: Array[Int]): List[TreeNode] = {
      def helper(root: TreeNode, parent: TreeNode): List[TreeNode] = {
        lazy val dfs = helper(root.left, root) ++ helper(root.right, root)
        if(root == null) List()
        else if (to_delete.contains(root.value)) {
          if (parent != null) {
            if (parent.left != null && parent.left.value == root.value) parent.left = null
            if (parent.right != null && parent.right.value == root.value) parent.right = null
          }
          List(root.left, root.right) ++ dfs
        }
        else dfs
      }

      (root +: helper(root, null)).filterNot { node => (node == null) || to_delete.contains(node.value) }
    }

    def firstMatching(str1: String, str2: String): String = {
      if (str1.length < str2.length) firstMatching(str2, str1)
      if (str2.isEmpty) return ""
      if (str1.startsWith(str2)) str2
      else firstMatching(str1, str2.take(str2.length - 1))
    }

    def findDirection(root: TreeNode, target: Int): String = {
      @tailrec
      def helper(queue: List[(TreeNode, String)]): String = {
        queue match {
          case Nil => ""  // If the queue is empty, return an empty string (target not found)
          case (node, path) :: rest =>
            if (node.value == target) path
            else {
              val newQueue = rest ++ (
                Option(node.left).map((_, path + "L")).toList ++
                  Option(node.right).map((_, path + "R")).toList
                )
              helper(newQueue)
            }
        }
      }

      helper(List((root, "")))
    }
}
