import com.ericsson.otp.erlang.*;


public class ServerModel {
	
	
	ActionsModel AM = new ActionsModel();
	 
	public static void main(String[] args) throws Exception {
		
		// TODO Auto-generated method stub
		
		ServerModel sm = new ServerModel();
		
		sm.serverModel();
		
				

	}
	public void serverModel()  throws Exception

    {
		
		   OtpNode myNode = new OtpNode("serverDM","erlang");
           OtpMbox myMbox = myNode.createMbox("boxDM");           
           OtpErlangObject myObject;
           OtpErlangTuple myMsg;
           OtpErlangPid from;
           OtpErlangString command;
           OtpErlangDouble opt;
           OtpErlangDouble medRul;
           OtpErlangDouble medProg;
           OtpErlangDouble RuleRat;
           OtpErlangDouble ProgRat;
           OtpErlangString rule;
           OtpErlangString act;
           
           OtpErlangString prev1;
           OtpErlangString prev2;
           OtpErlangString prev3;
           
           OtpErlangDouble size;
           OtpErlangLong arity;
           OtpErlangDouble cob;
           OtpErlangDouble cobNeg;
           OtpErlangLong PrevOps;
           
           OtpErlangDouble vars;
           OtpErlangDouble cons;
           OtpErlangDouble funcs;
           OtpErlangDouble strucs;
           OtpErlangDouble rec;
           
           
           OtpErlangAtom myAtom = new OtpErlangAtom("ok");
           OtpErlangAtom myAtom2 = new OtpErlangAtom("okModelo");
           Integer counter = 0;
           
           OtpErlangString op1 = new OtpErlangString("Model");
           OtpErlangString op2 = new OtpErlangString("Classify");
           
           
      while(counter >= 0) try

           {

                   myObject = myMbox.receive();
                   myMsg = (OtpErlangTuple) myObject;
                   from = (OtpErlangPid) myMsg.elementAt(0);
                   command = (OtpErlangString) myMsg.elementAt(1);
                  // System.out.println("command: "+command);
                  // System.out.println("1:"+command.equals(op1));
                  // System.out.println("2:"+command.equals(op2));
                   
                   if (command.equals(op1))
                   {
                	  System.out.println("Genero Modelo ");
                	  AM.generate_model();
                	  System.out.println("Modelo Generado ");
                	  OtpErlangObject[] reply = new OtpErlangObject[2];
                      reply[0] = myAtom;
                      reply[1] = myAtom2;
                      OtpErlangTuple myTuple = new OtpErlangTuple(reply);
                      myMbox.send(from, myTuple);
               	   
                   }
                   
                   else if (command.equals(op2))
                   {
                	   opt = (OtpErlangDouble) myMsg.elementAt(2);
                	   medRul = (OtpErlangDouble) myMsg.elementAt(3);
                       medProg = (OtpErlangDouble) myMsg.elementAt(4);
                       RuleRat = (OtpErlangDouble) myMsg.elementAt(5);
                       ProgRat = (OtpErlangDouble) myMsg.elementAt(6);
                	   rule = (OtpErlangString) myMsg.elementAt(7);
                	   size = (OtpErlangDouble) myMsg.elementAt(8);
                	   arity = (OtpErlangLong) myMsg.elementAt(9);
                	   act = (OtpErlangString) myMsg.elementAt(10);
                	   prev1 = (OtpErlangString) myMsg.elementAt(11);
                	   prev2 = (OtpErlangString) myMsg.elementAt(12);
                	   prev3 = (OtpErlangString) myMsg.elementAt(13);
                	   
                	   cob = (OtpErlangDouble) myMsg.elementAt(14);
                	   cobNeg = (OtpErlangDouble) myMsg.elementAt(15);
                	   PrevOps = (OtpErlangLong) myMsg.elementAt(16);
                	   
                	   vars = (OtpErlangDouble) myMsg.elementAt(17);
                	   cons = (OtpErlangDouble) myMsg.elementAt(18);
                	   funcs = (OtpErlangDouble) myMsg.elementAt(19);
                	   strucs = (OtpErlangDouble) myMsg.elementAt(20);
                	   rec = (OtpErlangDouble) myMsg.elementAt(21);
                       
                	  //System.out.println("Instancia Dentro de Server: "+Float.parseFloat(opt.toString())+" "+Float.parseFloat(medRul.toString())+" "+Float.parseFloat(medProg.toString())+" "+Integer.parseInt(vars.toString())+" "+Integer.parseInt(arity.toString())+" "+act.toString());
                	   float classified_class =  AM.classify_instance(Float.parseFloat(opt.toString()),Float.parseFloat(medRul.toString()),
                			   Float.parseFloat(medProg.toString()),Float.parseFloat(RuleRat.toString()),Float.parseFloat(ProgRat.toString()),rule.stringValue(), 
                			   Float.parseFloat(size.toString()),Integer.parseInt(arity.toString()),act.stringValue(),prev1.stringValue(),prev2.stringValue(),
                			   prev3.stringValue(),Float.parseFloat(cob.toString()),Float.parseFloat(cobNeg.toString()),Integer.parseInt(PrevOps.toString()),
                			   Float.parseFloat(vars.toString()),Float.parseFloat(cons.toString()),Float.parseFloat(funcs.toString()),Float.parseFloat(strucs.toString()),
                			   Float.parseFloat(rec.toString()));
                	  
                	   OtpErlangObject[] reply = new OtpErlangObject[2];
                       reply[0] = myAtom;
                       reply[1] = new OtpErlangFloat(classified_class);
                       OtpErlangTuple myTuple = new OtpErlangTuple(reply);
                       myMbox.send(from, myTuple);
                	   
                   }
                	   
                 

                   

	} catch(OtpErlangExit e)

             {

                   break;

             }

   }

}
