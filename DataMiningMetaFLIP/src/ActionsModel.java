import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.util.Enumeration;
import java.util.Random;

import weka.classifiers.Classifier.*;
import weka.classifiers.functions.LinearRegression;
import weka.classifiers.lazy.IBk;
import weka.classifiers.lazy.KStar;
import weka.core.*;
import weka.classifiers.trees.M5P;
import weka.classifiers.trees.m5.M5Base;



public class ActionsModel {

	/**
	 * @param args
	 */
	
	//private M5P model;
	//private LinearRegression model;
	private M5P model;
	private int rep=10;
	private int folds=10;
	private Instances instances;
	
//	public LinearRegression getModel() {
//		return model;
//	}
	
	
	
	private int global_optimality = 0;
	private int medRules = 1;
	private int medProgs = 2;
	private int ruleRat = 3;
	private int progRat = 4;
	
	private int rule_name =5;
	private int size =6;
	private int arity =7;
	private int action =8;
	private int prev1 = 9;
	private int prev2 = 10;
	private int prev3 = 11;
	
	private int cob = 12;
	private int cobN = 13;
	private int numPrev	= 14;
	private int vars = 15;
	private int cons = 16;
	private int funcs = 17;
	private int strucs = 18;
	private int rec = 19;
	private int clase= 20;
	
	
	public ActionsModel() {
		super();
	}

	
	
	
	public static void main(String[] args) throws Throwable {
		
		// TODO Auto-generated method stub
		
		
		//ActionsModel AM = new ActionsModel();
		//AM.generate_model();
		//float classified_class = (float) AM.classify_instance(0.8,"4",2,4,"3", 0.11);
		//System.out.println("Class: "+classified_class);

		//AM.generate_model();
		//float classified_class2 = (float) AM.classify_instance(0.1,"4",2,1,"1", 0.11);
		//System.out.println("Class2: "+classified_class2);
		//AM.generate_model();
		//float classified_class3 = (float) AM.classify_instance(0.8,"4",2,4,"3", 0.11);
		//System.out.println("Class: "+classified_class);
		
	

	}
	

	public void generate_model() throws Exception
	{
		// Read all the instances in the file 
		//FileReader reader = new FileReader("C:\\Users\\Nando\\workspace\\IFPER\\src\\Actions.arff"); 
		FileReader reader = new FileReader("..\\IFPER\\src\\Actions.arff"); 
		instances = new Instances(reader); 
		
		// Make the last attribute be the class 
		instances.setClassIndex(instances.numAttributes() - 1); 
		//model = new IBk(2);
		//model = new LinearRegression();
		model = new M5P();
		model.buildClassifier(instances);
		reader.close();
		//System.out.println("Instances: "+instances);
	}
	
	public float classify_instance(double Opt, double MedRules, double MedProgs, double RuleRat, double ProgRat,
			String Rule, double Size, int Arity, String Act, String Prev1, String Prev2, String Prev3, double Cob, double CobNeg, int NumPrev,
			double Vars, double Cons, double Funcs, double Strucs, double Rec) throws Exception
	{
		
		//System.out.println("-----------------> "+Opt+" "+medRules+" "+medProgs+" "+RuleRat+" "+ProgRat+" "+Rule+" "+Vars+" "+Arity+" "+Act+" "+Cob);

		//Instances dataset()
		Instance newInstance = instances.instance(1);
		newInstance.setValue(global_optimality, Opt); 
		//System.out.println("Instance: "+newInstance);
		newInstance.setValue(medRules, MedRules); 
		//System.out.println("Instance: "+newInstance);
		newInstance.setValue(medProgs, MedProgs); 
		//System.out.println("Instance: "+newInstance);
		newInstance.setValue(ruleRat, RuleRat); 
		//System.out.println("Instance: "+newInstance);
		newInstance.setValue(progRat, ProgRat); 
		//System.out.println("Instance: "+newInstance);
		newInstance.setValue(rule_name, Rule.toString()); 
		//System.out.println("Instance: "+newInstance);
		newInstance.setValue(size, Size); 
		//System.out.println("Instance: "+newInstance);
		newInstance.setValue(arity, Arity); 
		//System.out.println("Instance: "+newInstance);
		newInstance.setValue(action, Act.toString()); 
		//System.out.println("Instance: "+newInstance);
		newInstance.setValue(prev1, Prev1.toString());
		
		newInstance.setValue(prev2, Prev2.toString()); 
		
		newInstance.setValue(prev3, Prev3.toString()); 
		newInstance.setValue(cob, Cob); 
		newInstance.setValue(cobN, CobNeg);
		newInstance.setValue(numPrev, NumPrev);
		
		newInstance.setValue(vars, Vars);
		newInstance.setValue(cons, Cons);
		newInstance.setValue(funcs, Funcs);
		newInstance.setValue(strucs, Strucs);
		newInstance.setValue(rec, Rec);
		
		//System.out.println("Instance: "+newInstance);
		newInstance.setClassValue(0.0); 
		//System.out.println("Instance: "+newInstance);
		//inst.setDataset(); 
		System.out.println("Instance: "+newInstance);
		float classified_class = (float)model.classifyInstance(newInstance);
		//System.out.println("Class: "+classified_class);
		return classified_class;
		
	}
}
