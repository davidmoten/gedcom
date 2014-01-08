Scala project to parse gedcom 5.5 and 5.5.1 files into an abstract syntax tree (AST).

Usage:

    import com.github.davidmoten.gedcom._
    
    val tree = new Tree(inputStream)
    
    //get the abstract syntax tree to be traversed as you wish
    val ast = tree.root
    
    //tree.refs and tree.ref are convenience methods that point 
    //xref links to the relevant tree nodes
    println(tree.refs.mkString("\n"))

Status: *pre-alpha*