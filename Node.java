
/***************************************************/
/*
 
 Vinay Suri
 COEN 385
 Optional Project - Varient 1
 
 Node.java
 
 */
/***************************************************/

class Node {
    
    public String Value;
    char Name; //alphabet
    
    public Node Parent;
    public Node ChildRight;
    public Node ChildLeft;
    public int fun;
    
    public boolean hasParent;
    public boolean hasRight;
    public boolean hasLeft;
  
    public Node(){
        
        Parent = null;
        ChildRight = null;
        ChildLeft = null;
        fun=0;
        Name = ' ';
        Value = "";
        hasParent=false;
        hasRight=false;
        hasLeft=false;
    }

    public Node(Node P, char N, String val, int f) {
        
        Parent = P;
        Name = N;
        Value = val;
        fun = f;
        
        hasParent=true;
        hasRight=false;
        hasLeft=false;
    }
    public void SetName(char N)
    {
        Name = N;
    }
    public void SetParent(Node P)
    {
        Parent = P;
        hasParent=true;
    }
    
    public void SetChild(Node C)
    {
        if(!hasLeft)
        {
            ChildLeft = C;
            hasLeft=true;
        }
        else if(!hasRight)
        {
            ChildRight = C;
            hasRight=true;
        }
        else
            System.out.println("Already has two children...");
    }
    public void SetChildRight(Node R)
    {
        ChildRight = R;
        hasRight=true;
    }
    
    public void SetChildLeft(Node C)
    {
        ChildLeft = C;
        hasLeft=true;
    }
    
    public void SetValue(String Val)
    {
        Value = Val;
    }
    
    public String GetValue()
    {
        return Value;
    }
 
    public Node GetChildRight()
    {
        return ChildRight;
    }
    
    public Node GetChildLeft()
    {
        return ChildLeft;
    }
    
    public Node GetParent()
    {
        return Parent;
    }
}

