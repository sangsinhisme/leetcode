class TreeNode(_value: Int = 0, _left: TreeNode = null, _right: TreeNode = null) {
  var value: Int = _value
  var left: TreeNode = _left
  var right: TreeNode = _right

  def stringToTreeNode(data: String): TreeNode = {
    if (data.isEmpty || data == "[]") return null

    val values = data.stripPrefix("[").stripSuffix("]").split(",").map(_.trim).map {
      case "null" => None
      case v => Some(v.toInt)
    }

    val root = new TreeNode(values(0).get)
    val queue = scala.collection.mutable.Queue[TreeNode]()
    queue.enqueue(root)
    var i = 1

    while (i < values.length) {
      val current = queue.dequeue()

      if (values(i).isDefined) {
        current.left = new TreeNode(values(i).get)
        queue.enqueue(current.left)
      }
      i += 1

      if (i < values.length && values(i).isDefined) {
        current.right = new TreeNode(values(i).get)
        queue.enqueue(current.right)
      }
      i += 1
    }

    root
  }

  // Function to print tree in a readable way
  def printTree(root: TreeNode): Unit = {
    if (root == null) return
    val queue = scala.collection.mutable.Queue[TreeNode]()
    queue.enqueue(root)
    while (queue.nonEmpty) {
      val node = queue.dequeue()
      if (node == null) {
        print("null ")
      } else {
        print(s"${node.value} ")
        queue.enqueue(node.left)
        queue.enqueue(node.right)
      }
    }
  }
}
