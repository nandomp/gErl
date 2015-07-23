import com.ericsson.otp.erlang.OtpConnection;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpPeer;
import com.ericsson.otp.erlang.OtpSelf;


public class ErlConnection {

	private static OtpConnection conn;
	 public OtpErlangObject received;
	 private final String peer;
	 private final String cookie;
	 
	 public static void main(String []args){
		 new ErlConnection("metaflip_node@Nando-DSIC","erlang");
	 }

	  public ErlConnection(String _peer, String _cookie) {
		  peer = _peer;
		  cookie = _cookie;
		  connect();

           /*Do Calls to Rpc methods and then close the connection*/
		  disconnect();

	  }

	  private void connect() {
	   System.out.print("Please wait, connecting to "+peer+"....\n");

	   String javaClient ="java";
	   try {
	     OtpSelf self = new OtpSelf(javaClient, cookie.trim());
	     OtpPeer other = new OtpPeer(peer.trim());
	     conn = self.connect(other);
	    
	     conn.sendRPC("main","main",new OtpErlangList());
	     OtpErlangObject received = conn.receiveRPC(); 
	    
	     System.out.println("Connection Established with "+peer+"\n");
	     System.out.println("Recibido: "+received+"\n");
	   }
	   catch (Exception exp) {
	     System.out.println("connection error is :" + exp.toString());
	     exp.printStackTrace();
	   }

	 }

	 public void disconnect() {
	   System.out.println("Disconnecting....");
	   if(conn != null){
	     conn.close();
	   }
	   System.out.println("Successfuly Disconnected");
	 }

}

