/*  MicroJava Parser (HM 06-12-28)
    ================
*/
package MJ;

import java.util.*;
import MJ.SymTab.*;
import MJ.CodeGen.*;

public class Parser {
    private static final int  // token codes
        none      = 0, ident     = 1, number    = 2, charCon   = 3,
        plus      = 4, minus     = 5, times     = 6, slash     = 7,
        rem       = 8, eql       = 9, neq       = 10, lss       = 11,
        leq       = 12, gtr       = 13, geq       = 14, assign    = 15,
        semicolon = 16, comma     = 17, period    = 18, lpar      = 19,
        rpar      = 20, lbrack    = 21, rbrack    = 22, lbrace    = 23,
        rbrace    = 24, class_    = 25, else_     = 26, final_    = 27,
        if_       = 28, new_      = 29, print_    = 30, program_  = 31,
        read_     = 32, return_   = 33, void_     = 34, while_    = 35,
        eof       = 36;
    private static final String[] name = { // token names for error messages
        "none", "identifier", "number", "char constant", "+", "-", "*", "/", "%",
        "==", "!=", "<", "<=", ">", ">=", "=", ";", ",", ".", "(", ")",
        "[", "]", "{", "}", "class", "else", "final", "if", "new", "print",
        "program", "read", "return", "void", "while", "eof"
        };

    private static Token t;			// current token (recently recognized)
    private static Token la;		// lookahead token
    private static int sym;			// always contains la.kind
    public  static int errors;  // error counter
    private static int errDist;	// no. of correctly recognized tokens since last error
    private static Obj curMethod;

    private static BitSet exprStart, statStart, statSeqFollow, declStart, declFollow;

    //------------------- auxiliary methods ----------------------
    private static void scan() {
        t = la;
        la = Scanner.next();
        sym = la.kind;
        errDist++;
        /*
        System.out.print("line " + la.line + ", col " + la.col + ": " + name[sym]);
        if (sym == ident) System.out.print(" (" + la.string + ")");
        if (sym == number || sym == charCon) System.out.print(" (" + la.val + ")");
        System.out.println();*/
    }

    private static void check(int expected) {
        if (sym == expected) scan();
        else {
//            System.out.println("found " + name[sym]);
            error(name[expected] + " expected");
        }
    }

    public static void error(String msg) { // syntactic error at token la
        if (errDist >= 3) {
                System.out.println("-- line " + la.line + " col " + la.col + ": " + msg);
                errors++;
        }
        errDist = 0;
    }

    //-------------- parsing methods (in alphabetical order) -----------------   
    //"(" [ Expr {"," Expr} ] ")".
    private static void ActPars(Operand m) {
        Operand ap;
        check(lpar);
        if (m.kind != Operand.Meth) { error("called object is not a method"); m.obj = Tab.noObj; }
        else{
            int aPars = 0;
            int fPars = m.obj.nPars;
            Obj fp = m.obj.locals;

            for(;;){
                if(sym == comma)scan();
                else if(sym == minus || exprStart.get(sym)){
                    ap = Expr();
                    Code.load(ap); aPars++;
                    if (fp != null) {
                        if (!ap.type.assignableTo(fp.type)) error("parameter type mismatch");
                    fp = fp.next;}
                }else break;
            }

            if (aPars > fPars) error("more actual than formal parameters");
            else if (aPars < fPars) error("fewer actual than formal parameters");
        }
        check(rpar);
    }
    
    //"+" | "-".
    private static void Addop() {
        if(sym == plus ) scan();
        else if(sym == minus) scan();
        else error("Error: Invalid Addop");
    }

    //"{" {Statement} "}"
    private static void Block() {
        check(lbrace);        
        while(sym != rbrace && sym != eof){
            Statement();
            }
        check(rbrace);
        }
    

    //"class" ident "{" {VarDecl} "}".
    private static void ClassDecl() {
        check(class_);
        check(ident);

        Struct curClass = new Struct(Struct.Class);
        Tab.insert(Obj.Type, t.stringBuilder.toString(), curClass);
        
        Tab.openScope();
        check(lbrace);
        while(sym == ident){
            VarDecl();
        }
        
        curClass.nFields = Tab.curScope.nVars;
        curClass.fields = Tab.curScope.locals;
        check(rbrace);
        Tab.closeScope();
    }

    //Expr Relop Expr.
    private static int Condition() {
        int op;
        Operand x,y;
        x = Expr(); Code.load(x);
        op = Relop();
        y = Expr(); Code.load(y);
        if (!x.type.compatibleWith(y.type)) error("type mismatch"); 
        if (x.type.isRefType() && op != Code.eq && op != Code.ne) error("invalid compare");
        return op;
    }
    
    //"final" Type ident "=" (number | charConst) ";".
    private static void ConstDecl() {
        check(final_);
        Struct type = Type();
        check(ident);
        Obj obj = Tab.insert(Obj.Con, t.stringBuilder.toString(), type);
        
        check(assign);
        if(sym == number){
            if(type != Tab.intType){error("Character Constant Expected");}
            scan();
            obj.val = t.val; 
        }else if(sym == charCon){
            if(type != Tab.charType){error("Integer Constant Expected");}
            scan();
            obj.val = t.val; 
        }else error("Error: Invalid ConstDecl");
        check(semicolon);
    }
    
    //ident {'.' ident | '[' Expr ']'}.
     private static Operand Designator() {
         Operand x, y; Obj o;
         check(ident);
         x = new Operand(Tab.find(t.stringBuilder.toString()));
         for (;;)
             if (sym == period) {
                 Code.load(x);
                 scan();
                 check(ident);
                 if (x.type.kind == Struct.Class) {
                     o = Tab.findField(t.stringBuilder.toString(), x.type);
                     x.adr = o.adr;
                     x.type = o.type;
                 } else error("dereferenced object is not a class");
                 x.kind = Operand.Fld;
             } else if (sym == lbrack) {
                 Code.load(x);
                 scan();
                 y = Expr();
                 check(rbrack);
                 if (x.type.kind == Struct.Arr) {
                     if (y.type != Tab.intType) error("index must be an int");
                     Code.load(y);
                     x.type = x.type.elemType;
                 } else error("indexed object is not an array");
                 x.kind = Operand.Elem;
             } else break;
         return x;
     }

    //["-"] Term {Addop Term}.
    private static Operand Expr() {
        Operand x,y;
        int op;
        if(sym == minus){
            scan();
            x = Term();
            if(x.type != Tab.intType)error("integer operand required");
            if(x.kind == Operand.Con) x.val = -x.val;
            else {Code.load(x); Code.put(Code.neg);} 
        } else x = Term();
        
        for (;;) {
            if (sym == plus) { scan(); op = Code.add;
            }else if (sym == minus) { scan(); op = Code.sub;
            }else break;
            Code.load(x);
            y = Term();
            Code.load(y);
            if (x.type != Tab.intType || y.type != Tab.intType) error("operands must be of type int"); 
            Code.put(op);
        }
        return x;
    }
    
    //Designator [ActPars]
    //| number
    //| charConst
    //| "new" ident ["[" Expr "]"]
    //| "(" Expr ")".
    private static Operand Factor() {
        Operand x;
        
        if(sym == ident){
            x = Designator();
            if(sym == lpar){
                ActPars(x);
                if (x.type == Tab.noType) error("procedure called as a function"); 
                if (x.obj == Tab.ordObj || x.obj == Tab.chrObj) ; // nothing 
                else if (x.obj == Tab.lenObj) Code.put(Code.arraylength); 
                else { Code.put(Code.call); Code.put2(x.adr); } 
                x.kind = Operand.Stack;
            }
        }else if(sym == number){
            x = new Operand(t.val);
            scan();
        }else if(sym == charCon){
            x = new Operand(t.val);
            x.type = Tab.charType;
            scan();
        }else if(sym == new_){
            scan();
            check(ident);
            Obj obj = Tab.find(t.stringBuilder.toString());
            Struct type = obj.type; 
           
            if(sym == lbrack) {
                scan(); 
                if (obj.kind != Obj.Type) error("type expected");
                x = Expr();
                if (x.type != Tab.intType) error("array size must be an integer"); 
                Code.load(x); 
                Code.put(Code.newarray); 
                if (type == Tab.charType) Code.put(0); 
                else Code.put(1); type = new Struct(Struct.Arr, type);
                check(rbrack);}
            else{
                 if (obj.kind != Obj.Type || type.kind != Struct.Class) error("class type expected"); 
                 Code.put(Code.new_); Code.put2(type.nFields);}
            
            x = new Operand(Operand.Stack,0,type);
        }else if(sym == lpar){
            x = Expr();
            check(rpar);
        }else{
            error("Error: Factor");
            x = new Operand(Operand.Stack, 0, Tab.noType);
        }
        return x;
    }
    
    //Type ident {"," Type ident}.
    private static int FormPars() {
        int n = 0;
        Struct type = Type();
        check(ident);
        Tab.insert(Obj.Var, t.stringBuilder.toString(), type);
        n++;
        while(sym==comma){
            scan();
            type = Type();
            check(ident);
            Tab.insert(Obj.Var, t.stringBuilder.toString(), type);
            n++;
        }
        return n;
    }
    
    //(Type | "void") ident "(" [FormPars] ")" {VarDecl} Block.
    private static void MethodDecl() {
        Struct type = Tab.noType;
        if(sym == ident) type = Type();
        else if(sym == void_) scan();
        else error("Error: MethodDecl");
        
        check(ident);
        String name = t.stringBuilder.toString();
        curMethod = Tab.insert(Obj.Meth, t.stringBuilder.toString(), type);
        Tab.openScope();
        check(lpar);
        
        if(sym == ident) curMethod.nPars = FormPars();
        if (name.equals("main")) {
            Code.mainPc = Code.pc;
            if (curMethod.type != Tab.noType) error(" main method must be void"); 
            if (curMethod.nPars != 0) error("main must not have parameters");}
        
        check(rpar);
        while(sym == ident){
            VarDecl();
        }
        
        curMethod.locals = Tab.curScope.locals;
        curMethod.adr = Code.pc; 
        Code.put(Code.enter); 
        Code.put(curMethod.nPars); 
        Code.put(Tab.curScope.nVars); 
        
        Block();
        
        if (curMethod.type == Tab.noType) { 
            Code.put(Code.exit);
            Code.put(Code.return_);
        } else { // end of function reached without a return statement 
            Code.put(Code.trap); Code.put(1); } 
        Tab.closeScope();
    }
    
    //"*" | "/" | "%"
    private static void Mulop() {
        if(sym == times) scan();
        else if(sym == slash) scan();
        else if(sym == rem) scan();
        else error("Error: Invalid Mulop");
    }
    
    //"program" ident {ConstDecl | ClassDecl | VarDecl} '{' {MethodDecl} '}'.
    private static void Program() {
        check(program_);
        check(ident);
        Tab.openScope();
        for (;;) {
            if (sym == final_) ConstDecl();
            else if (sym == class_) ClassDecl();
            else if (sym == ident) VarDecl();
            else if (sym == lbrace || sym == eof) break;
            else {
                error("invalid start of Declaration");
                do scan(); 
                while (sym != final_ && sym != class_ && sym != lbrace && sym != eof && sym != semicolon);
                    if (sym == semicolon) scan();
                        errDist = 0;
            }
        }
        check(lbrace);
        while(sym == ident ||sym == void_){
            MethodDecl();
        }
        
        check(rbrace);
        Tab.dumpScope(Tab.curScope.locals);
        Tab.closeScope();
        Code.dump();
    }

    //"==" | "!=" | ">" | ">=" | "<" | "<=".
    private static int Relop() {
        int op = -1;
        if(sym == eql){scan(); op = Code.eq;
        }else if(sym == neq){scan(); op = Code.ne;
        }else if(sym == gtr){scan(); op = Code.gt;
        }else if(sym == geq){scan(); op = Code.ge;
        }else if(sym == lss){scan(); op = Code.lt;
        }else if(sym == leq){scan(); op = Code.le;
        }else{error("Error: Invalid Relop");   
        }
        return op;
    }

    //Designator ("=" Expr | ActPars) ";"
    //| "if" "(" Condition ")" Statement ["else" Statement]
    //| "while" "(" Condition ")" Statement
    //| "return" [Expr] ";"
    //| "read" "(" Designator ")" ";"
    //| "print" "(" Expr ["," number] ")" ";"
    //| Block
    //| ";".
    private static void Statement() {
        Operand x,y;
        int op;
        if (!statStart.get(sym)) {
            error("invalid start of statement");
            do scan(); 
            while (!statStart.get(sym) && sym!=rbrace && sym != semicolon);
        if (sym == semicolon) scan();
            errDist = 0;
        }
        
        if(sym == ident){
            x = Designator();
            if(sym == assign){
                scan();
                y = Expr();
                if (y.type.assignableTo(x.type)) Code.assign(x, y); // x: Local | Static | Fld | Elem
                    // assign must load y
                else error("incompatible types in assignment");
            }else if(sym == lpar){
                ActPars(x);
                Code.put(Code.call);
                Code.put2(x.adr);
                if (x.type != Tab.noType) Code.put(Code.pop);
            }
            check(semicolon);
        }else if(sym == if_){
            scan();
            check(lpar); op = Condition();
            Code.putFalseJump(op, 0);
            int adr = Code.pc - 2;
            check(rpar);
            Statement();

            if (sym == else_) { 
                scan(); 
                Code.putJump(0);
                int adr2 = Code.pc - 2;
                Code.fixup(adr);
                Statement(); 
                Code.fixup(adr2);    
            }else Code.fixup(adr);
        }else if(sym == while_){
            scan();
            int top = Code.pc;
            check(lpar); 
            op = Condition(); 
            Code.putFalseJump(op, 0);
            int adr = Code.pc - 2;
            check(rpar);
            Statement();
            Code.putJump(top);
            Code.fixup(adr);
        }else if(sym == return_){
            scan();
            if(exprStart.get(sym)){
                x = Expr();
                Code.load(x);
                if (curMethod.type == Tab.noType) error("void method must not return a value");
                else if (!x.type.assignableTo(curMethod.type)) error("return type must match method type");
            }else if (curMethod.type != Tab.noType) error("return expression expected");
            Code.put(Code.exit);
            Code.put(Code.return_);
            check(semicolon);
        }else if(sym == read_){
            scan();
            check(lpar); 
            x = Designator();
            if(x.type.kind != Struct.Int && x.type.kind != Struct.Char) error("Can only read int or char variables");
            check(rpar);
            check(semicolon);
        }else if(sym == print_){
            scan();
            check(lpar); 
            x = Expr();
            if(x.type.kind != Struct.Int && x.type.kind != Struct.Char) error("Can only print int or char variables");
            if(sym == comma){
                scan();
                check(number);
            }
            check(rpar);
            check(semicolon);
        }else if(sym == lbrace) Block();
        else if(sym == semicolon) scan();
    }
    
    //Factor {Mulop Factor}.
    private static Operand Term() {
        Operand x,y;
        int op;
        x = Factor();
        
        for (;;) {
            if (sym == times){op = Code.mul;scan();
            }else if (sym == slash) {op = Code.div;scan();
            }else if (sym == rem) {op = Code.rem;scan();
            }else break;
            
            Code.load(x);
            y = Factor();
            Code.load(y);
            if (x.type != Tab.intType || y.type != Tab.intType) error("operands must be of type int"); 
            Code.put(op);
        }
        return x;
    }
        
    //ident ["[" "]"].
    private static Struct Type() {
        check(ident);
        Obj obj = Tab.find(t.stringBuilder.toString());
        if(obj.kind != obj.Type) error("Type Expected");
        Struct type = obj.type;
        if(sym == lbrack){
            scan();
            check(rbrack);
            type = new Struct(Struct.Arr,type);
        }
        return type;
    }

    //Type ident {"," ident } ";".
    private static void VarDecl() {
        Struct type;
        type = Type();
        check(ident);
        Tab.insert(Obj.Var, t.stringBuilder.toString(), type);
        while(sym == comma){
            scan();
            check(ident);
            Tab.insert(Obj.Var, t.stringBuilder.toString(), type);
        }
        check(semicolon);
    } 
 
    public static void parse() {
        // initialize symbol sets
        BitSet s;
        s = new BitSet(64); exprStart = s;
        s.set(ident); s.set(number); s.set(charCon); s.set(new_); s.set(lpar); s.set(minus);

        s = new BitSet(64); statStart = s;
        s.set(ident); s.set(if_); s.set(while_); s.set(read_);
        s.set(return_); s.set(print_); s.set(lbrace); s.set(semicolon);

        s = new BitSet(64); statSeqFollow = s;
        s.set(rbrace); s.set(eof);

        s = new BitSet(64); declStart = s;
        s.set(final_); s.set(ident); s.set(class_);

        s = new BitSet(64); declFollow = s;
        s.set(lbrace); s.set(void_); s.set(eof);

        // start parsing
        Tab.init();
        Code.init();
        errors = 0; errDist = 3;
        scan();
        Program();
        if (sym != eof) error("end of file found before end of program");
    }
}