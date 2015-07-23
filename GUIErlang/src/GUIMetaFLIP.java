import com.ericsson.otp.erlang.*;

import java.awt.BorderLayout;
import java.awt.EventQueue;

import javax.swing.JFrame;
import javax.swing.JPanel;
import javax.swing.border.EmptyBorder;
import javax.swing.filechooser.FileNameExtensionFilter;
import javax.swing.DefaultListModel;
import javax.swing.JFileChooser;
import javax.swing.JMenuBar;
import javax.swing.JMenu;
import javax.swing.JMenuItem;
import javax.swing.JOptionPane;
import javax.swing.JScrollPane;
import javax.swing.JTextPane;
import javax.swing.JButton;
import java.awt.event.ActionListener;
import java.awt.event.ActionEvent;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.io.*;

import javax.swing.JLabel;
import javax.swing.JTextField;
import javax.swing.JProgressBar;
import java.awt.Component;
import javax.swing.Box;
import java.awt.Panel;
import javax.swing.JList;
import javax.swing.AbstractListModel;
import javax.swing.GroupLayout;
import javax.swing.GroupLayout.Alignment;
import com.jgoodies.forms.layout.FormLayout;
import com.jgoodies.forms.layout.ColumnSpec;
import com.jgoodies.forms.factories.FormFactory;
import com.jgoodies.forms.layout.RowSpec;
import javax.swing.BoxLayout;
import java.awt.GridBagLayout;
import java.awt.GridBagConstraints;
import java.awt.Insets;
import java.awt.FlowLayout;
import javax.swing.SpringLayout;
import net.miginfocom.swing.MigLayout;
import java.awt.GridLayout;
import java.awt.Choice;
import javax.swing.JSpinner;
import javax.swing.JPopupMenu;
import java.awt.Color;
import javax.swing.UIManager;
import java.awt.Font;
import javax.swing.ImageIcon;
import javax.swing.JTabbedPane;
import javax.swing.JTextArea;
import javax.swing.JTable;
import javax.swing.table.DefaultTableModel;
import javax.swing.JCheckBox;
import java.net.*;





public class GUIMetaFLIP extends JFrame {

	private JPanel contentPane;
	private File PosEx;
	private File NegEx;
	private File PcySave;
	private File PcyLoad;
	private File[] MultiplePEX;
	private File[] MultipleNEX;
	private File[] MultiplePCY;
	private String PcyLoadPath = "";
	private String PcySavePath = "";	
	private File Operators;
	private JButton btnSteps;
	private JButton btnRules;
	static GUIMetaFLIP frame;
	private JLabel lblNewNEg;
	private JLabel lblNewLabelPos;
	private JLabel lblSteps;
	int steps = 10;
	
	
	static final String Select = "Sports";
    String input = "";
    DefaultListModel listModel;
    JList list;
    private JTextPane textPanePos;
    private JTextPane textPaneNeg;
    private JLabel lblTimeElapsed;
    private JScrollPane scrollPane_2;
    /**
     * @wbp.nonvisual location=-30,159
     */
    private final JTable table = new JTable();
    private JTable table_programs;
    private DefaultTableModel model = new DefaultTableModel();
    private JTable table_Rules;
    private DefaultTableModel modelRules = new DefaultTableModel();
    private JTable table_Operators;
    private DefaultTableModel modelOp = new DefaultTableModel();
    private JLabel lblNewOperators;
    private JCheckBox chckbxPrograms;
    private JTable table_atts;
    private DefaultTableModel modelAtts = new DefaultTableModel();
    private JTable table_SelectAtt;
    private DefaultTableModel modelSelected = new DefaultTableModel();
    private JScrollPane scrollPane_Atts;
    private JScrollPane scrollPane_SelectAtt;
    private JMenuItem mntmExport;
    private JMenuItem mntmImport;
    private JMenu mnPolicy;
    private JLabel lblLabelPcyLoaded;
    private JLabel lblPolicyS;
    private JLabel lblPcySaved;
    private JMenuItem mntmMultipleFiles;
    private JMenuItem mntmMultipleFilesSave;
    private JButton btnRunScript;
    
	/**
	 * Launch the application.
	 */
	public static void main(String[] args) {
		EventQueue.invokeLater(new Runnable() {
			public void run() {
				try {
					//GUIMetaFLIP frame = new GUIMetaFLIP();
					frame = new GUIMetaFLIP();
		
					frame.setVisible(true);
				} catch (Exception e) {
					e.printStackTrace();
				}
			}
		});
	}

	/**
	 * Create the frame.
	 */
	public GUIMetaFLIP() {
		
		setTitle("metaFLIP");
		setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		setBounds(100, 100, 1400, 720);
		
		JMenuBar menuBar = new JMenuBar();
		setJMenuBar(menuBar);
		
		JMenu mnFile = new JMenu("File");
		menuBar.add(mnFile);
		
		JMenuItem mntmRun = new JMenuItem("Run");
		mntmRun.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				try {
					RunAction();
				} catch (Exception e1) {
					// TODO Auto-generated catch block
					e1.printStackTrace();
				}
			}
		});
		
		mnFile.add(mntmRun);
		
		JMenuItem mntmExit = new JMenuItem("Exit");
		mntmExit.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				System.exit(0);
				
			}
		});
		
		mntmMultipleFiles = new JMenuItem("Multiple Files Load");
		mnFile.add(mntmMultipleFiles);
		
		mntmMultipleFilesSave = new JMenuItem("Multiple Files Save");
		mnFile.add(mntmMultipleFilesSave);
		mnFile.add(mntmExit);
		
		mntmMultipleFiles.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				JFileChooser selectorArchivo = new JFileChooser("..\\IFPER\\Multiple");
				selectorArchivo.setMultiSelectionEnabled(true);
				
				//selectorArchivo.setFileSelectionMode(JFileChooser.FILES_ONLY);
				javax.swing.filechooser.FileFilter filtro = new FileNameExtensionFilter("Policy Files (.pex)", "pex");
				selectorArchivo.setFileFilter(filtro);
				
				
				if (selectorArchivo.showOpenDialog(null) == JFileChooser.APPROVE_OPTION) {
					MultiplePEX = selectorArchivo.getSelectedFiles();
					//System.out.println (MultiplePEX[1]);
					//System.out.println (MultiplePEX[2]);
					System.out.println (MultiplePEX.length);
					
					JFileChooser selectorArchivoNEX = new JFileChooser("..\\IFPER\\Multiple");
					selectorArchivoNEX.setMultiSelectionEnabled(true);
					javax.swing.filechooser.FileFilter filtro2 = new FileNameExtensionFilter("Policy Files (.nex)", "nex");
					selectorArchivoNEX.setFileFilter(filtro2);
					
					if (selectorArchivoNEX.showOpenDialog(null) == JFileChooser.APPROVE_OPTION) {
						MultipleNEX = selectorArchivoNEX.getSelectedFiles();
					//	System.out.println (MultipleNEX[1]);
					//	System.out.println (MultipleNEX[2]);
						
						JFileChooser selectorArchivoPCY = new JFileChooser("..\\IFPER\\Multiple");
						selectorArchivoPCY.setMultiSelectionEnabled(true);
						javax.swing.filechooser.FileFilter filtro3 = new FileNameExtensionFilter("Policy Files (.pcy)", "pcy");
						selectorArchivoPCY.setFileFilter(filtro3);
						
						if (selectorArchivoPCY.showOpenDialog(null) == JFileChooser.APPROVE_OPTION) {
							MultiplePCY = selectorArchivoPCY.getSelectedFiles();
						//	System.out.println (MultiplePCY[1]);
						//	System.out.println (MultiplePCY[2]);
							
							
							try {
								RunActionMultiple_load();
							} catch (Exception ee) {
								// TODO Auto-generated catch block
								ee.printStackTrace();
							}
						}
					}
				}
						
		
			}
		});
		
		mntmMultipleFilesSave.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				JFileChooser selectorArchivo = new JFileChooser("..\\IFPER\\Multiple");
				selectorArchivo.setMultiSelectionEnabled(true);
				
				//selectorArchivo.setFileSelectionMode(JFileChooser.FILES_ONLY);
				javax.swing.filechooser.FileFilter filtro = new FileNameExtensionFilter("Policy Files (.pex)", "pex");
				selectorArchivo.setFileFilter(filtro);
				
				
				if (selectorArchivo.showOpenDialog(null) == JFileChooser.APPROVE_OPTION) {
					MultiplePEX = selectorArchivo.getSelectedFiles();
					//System.out.println (MultiplePEX[1]);
					//System.out.println (MultiplePEX[2]);
					System.out.println (MultiplePEX.length);
					
					JFileChooser selectorArchivoNEX = new JFileChooser("..\\IFPER\\Multiple");
					selectorArchivoNEX.setMultiSelectionEnabled(true);
					javax.swing.filechooser.FileFilter filtro2 = new FileNameExtensionFilter("Policy Files (.nex)", "nex");
					selectorArchivoNEX.setFileFilter(filtro2);
					
					if (selectorArchivoNEX.showOpenDialog(null) == JFileChooser.APPROVE_OPTION) {
						MultipleNEX = selectorArchivoNEX.getSelectedFiles();
					//	System.out.println (MultipleNEX[1]);
					//	System.out.println (MultipleNEX[2]);
						
							
							
							
							try {
								RunActionMultiple_save();
							} catch (Exception ee) {
								// TODO Auto-generated catch block
								ee.printStackTrace();
							}
						
					}
				}
						
		
			}
		});
		
		
		JMenu mnHelp = new JMenu("Help");
		menuBar.add(mnHelp);
		
		mnPolicy = new JMenu("Policy");
		menuBar.add(mnPolicy);
		
		mntmExport = new JMenuItem("Export");
		mnPolicy.add(mntmExport);
		mntmExport.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				JFileChooser selectorArchivo = new JFileChooser("..\\IFPER\\Policy");
				selectorArchivo.setFileSelectionMode(JFileChooser.FILES_ONLY);
				javax.swing.filechooser.FileFilter filtro = new FileNameExtensionFilter("Policy Files (.pcy)", "pcy");
				selectorArchivo.setFileFilter(filtro);
				
				
				if (selectorArchivo.showOpenDialog(null) == JFileChooser.APPROVE_OPTION) {
					PcySave = selectorArchivo.getSelectedFile();
					System.out.println (PcySave.getName());
					lblLabelPcyLoaded.setText(PcySave.getName());
					PcySavePath=PcySave.getAbsolutePath();
					
				
					
				}
			}
		});
		
		
		
		mntmImport = new JMenuItem("Import");
		mnPolicy.add(mntmImport);
		mntmImport.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				JFileChooser selectorArchivo = new JFileChooser("..\\IFPER\\Policy");
				selectorArchivo.setFileSelectionMode(JFileChooser.FILES_ONLY);
				javax.swing.filechooser.FileFilter filtro = new FileNameExtensionFilter("Policy Files (.pcy)", "pcy");
				selectorArchivo.setFileFilter(filtro);
				
				
				if (selectorArchivo.showOpenDialog(null) == JFileChooser.APPROVE_OPTION) {
					PcyLoad = selectorArchivo.getSelectedFile();
					System.out.println (PcyLoad.getName());
					lblPcySaved.setText(PcyLoad.getName());
					PcyLoadPath=PcyLoad.getAbsolutePath();
					
				
				}
			}
		});
		
		
		
		
		
		
		contentPane = new JPanel();
		contentPane.setBorder(new EmptyBorder(5, 5, 5, 5));
		setContentPane(contentPane);
		contentPane.setLayout(null);
		
		Panel panelMainButtoms = new Panel();
		panelMainButtoms.setBounds(10, 10, 785, 35);
		contentPane.add(panelMainButtoms);
		
		JButton buttonNeg = new JButton("Neg");
		buttonNeg.setIcon(new ImageIcon("ico\\minus_grey.png"));
		buttonNeg.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				JFileChooser selectorArchivo = new JFileChooser("..\\IFPER\\Instances");
				selectorArchivo.setFileSelectionMode(JFileChooser.FILES_ONLY);
				javax.swing.filechooser.FileFilter filtro = new FileNameExtensionFilter("Negative Examples (.nex)", "nex");
				selectorArchivo.setFileFilter(filtro);
				
				
				if (selectorArchivo.showOpenDialog(null) == JFileChooser.APPROVE_OPTION) {
					NegEx = selectorArchivo.getSelectedFile();
					System.out.println (NegEx.getName());
					lblNewNEg.setText("Nex: "+NegEx.getName());
					 FileReader fr;
					try {
						fr = new FileReader(NegEx);
						 BufferedReader bf = new BufferedReader(fr);
						 String sCadena ="";
						 String Neg="";
						 int val=0;
						 while ((sCadena = bf.readLine())!=null) {
							 Neg+=sCadena+"\n"; 							 
							 }
						
					     textPaneNeg.setText(Neg);
					} catch (FileNotFoundException e1) {
						// TODO Auto-generated catch block
						e1.printStackTrace();
					} catch (IOException e1) {
						// TODO Auto-generated catch block
						e1.printStackTrace();
					}
					 
					 
					
				}
			}
		});
		panelMainButtoms.setLayout(new GridLayout(0, 5, 0, 0));
		panelMainButtoms.add(buttonNeg);
		
		JButton buttonPos = new JButton("Pos");
		buttonPos.setIcon(new ImageIcon("ico\\plus_grey.png"));
		panelMainButtoms.add(buttonPos);
		buttonPos.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				JFileChooser selectorArchivo = new JFileChooser("..\\IFPER\\Instances");
				selectorArchivo.setFileSelectionMode(JFileChooser.FILES_ONLY);
				javax.swing.filechooser.FileFilter filtro = new FileNameExtensionFilter("Positive Examples (.pex)", "pex");
				selectorArchivo.setFileFilter(filtro);
				
				
				if (selectorArchivo.showOpenDialog(null) == JFileChooser.APPROVE_OPTION) {
					PosEx = selectorArchivo.getSelectedFile();
					System.out.println (PosEx.getName());
					lblNewLabelPos.setText("Pex: "+PosEx.getName());
				
					 FileReader fr;
						try {
							fr = new FileReader(PosEx);
							 BufferedReader bf = new BufferedReader(fr);
							 String sCadena ="";
							 String Pos="";
							 int val=0;
							 while ((sCadena = bf.readLine())!=null) {
								 Pos+=sCadena+"\n"; 							 
								 }
							
						     textPanePos.setText(Pos);
						} catch (FileNotFoundException e1) {
							// TODO Auto-generated catch block
							e1.printStackTrace();
						} catch (IOException e1) {
							// TODO Auto-generated catch block
							e1.printStackTrace();
						}	
				}
			
			}
		});
		
		btnSteps = new JButton("Steps");
		btnSteps.setIcon(new ImageIcon("ico\\wranch_grey.png"));
		btnSteps.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent arg0) {
			
				String s = (String)JOptionPane.showInputDialog(
	                    frame,
	                    "Numer of steps	",
	                    "Steps",
	                    JOptionPane.PLAIN_MESSAGE,
	                    null,
	                    null,
	                    "100");
	 
	        System.out.println("Steps "+s);
	        steps = Integer.parseInt(s);
	        lblSteps.setText("Steps: "+steps);
	                

			}
		});
		panelMainButtoms.add(btnSteps);
		
		btnRules = new JButton("Operators");
		btnRules.setIcon(new ImageIcon("ico\\wranch_grey.png"));
		btnRules.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent arg0) {
				JFileChooser selectorArchivo = new JFileChooser("..\\IFPER\\Operators");
				selectorArchivo.setFileSelectionMode(JFileChooser.FILES_ONLY);
				javax.swing.filechooser.FileFilter filtro = new FileNameExtensionFilter("Operators (.erl)", "erl");
				selectorArchivo.setFileFilter(filtro);
				
				
				if (selectorArchivo.showOpenDialog(null) == JFileChooser.APPROVE_OPTION) {
					Operators = selectorArchivo.getSelectedFile();
					System.out.println (Operators.getAbsolutePath());
					lblNewOperators.setText("Operators: "+Operators.getName());
					
				
					
				}
			
			}});
		
		panelMainButtoms.add(btnRules);
		
		chckbxPrograms = new JCheckBox("Programs");
		panelMainButtoms.add(chckbxPrograms);
		
		JPanel panel_Labels = new JPanel();
		panel_Labels.setBounds(10, 51, 785, 20);
		contentPane.add(panel_Labels);
		panel_Labels.setLayout(new GridLayout(1, 0, 0, 0));
		
		lblNewNEg = new JLabel("Nex: ");
		panel_Labels.add(lblNewNEg);
		
		lblNewLabelPos = new JLabel("Pex: ");
		panel_Labels.add(lblNewLabelPos);
		
		lblSteps = new JLabel("Steps: ");
		panel_Labels.add(lblSteps);
		
		lblNewOperators = new JLabel("Operators: ");
		panel_Labels.add(lblNewOperators);
		
		JLabel lblNewLabel_1 = new JLabel("");
		panel_Labels.add(lblNewLabel_1);
		
		JTabbedPane tabbedPaneMain = new JTabbedPane(JTabbedPane.TOP);
		tabbedPaneMain.setBounds(10, 82, 1364, 563);
		contentPane.add(tabbedPaneMain);
		
		
		JPanel panel_Statistics = new JPanel();
		tabbedPaneMain.addTab("Statistics", null, panel_Statistics, null);
		panel_Statistics.setBounds(10, 101, 785, 544);
		//contentPane.add(panelMain);
		panel_Statistics.setLayout(null);
		
		
		JLabel lblGeneratedRules = new JLabel("Generated Rules");
		lblGeneratedRules.setBounds(10, 11, 127, 14);
		panel_Statistics.add(lblGeneratedRules);
		
		JScrollPane scrollPane_Rules = new JScrollPane();
		scrollPane_Rules.setBounds(10, 30, 1339, 213);
		panel_Statistics.add(scrollPane_Rules);
		
		table_Rules = new JTable(modelRules);
		modelRules.addColumn("Rule ID");		
		modelRules.addColumn("Rule");
		modelRules.addColumn("Optimality");
		modelRules.addColumn("Operators");	
		modelRules.addColumn("CovE+");	
		modelRules.addColumn("PosCov");	
		modelRules.addColumn("CovE-");	
		modelRules.addColumn("NegCov");	
		modelRules.addColumn("Step");
		modelRules.addColumn("MML");
		modelRules.addColumn("MMLCov");
		
		table_Rules.setAutoCreateRowSorter(true);	
		scrollPane_Rules.setViewportView(table_Rules);
		
		JLabel lblOperatorStatistics = new JLabel("Operators Statistics");
		lblOperatorStatistics.setBounds(10, 254, 146, 14);
		panel_Statistics.add(lblOperatorStatistics);
		
		JScrollPane scrollPane_Actions = new JScrollPane();
		scrollPane_Actions.setBounds(10, 275, 1339, 213);
		panel_Statistics.add(scrollPane_Actions);
		
		table_Operators = new JTable(modelOp);
		modelOp.addColumn("Op ID");
		modelOp.addColumn("Operator");
		modelOp.addColumn("Reward");
		modelOp.addColumn("Times");
		table_Operators.setAutoCreateRowSorter(true);	
		scrollPane_Actions.setViewportView(table_Operators);
		
		lblTimeElapsed = new JLabel("Elapsed time: ");
		lblTimeElapsed.setBounds(10, 519, 288, 14);
		panel_Statistics.add(lblTimeElapsed);
		
		JPanel panel_Programs = new JPanel();
		tabbedPaneMain.addTab("Programs", null, panel_Programs, null);
		panel_Programs.setLayout(null);
		
		scrollPane_2 = new JScrollPane();
		scrollPane_2.setBounds(10, 11, 1339, 513);
		panel_Programs.add(scrollPane_2);
		
	
		table_programs = new JTable(model);
		model.addColumn("Program ID");
		model.addColumn("Rules Keys");
		model.addColumn("E+ Covered");
		model.addColumn("E- Covered");
		model.addColumn("Optimality");
		model.addColumn("Num Opers");
		model.addColumn("Step");
		table_programs.setAutoCreateRowSorter(true);	
		scrollPane_2.setViewportView(table_programs);
		
		JPanel panel_Instances = new JPanel();
		tabbedPaneMain.addTab("Instances", null, panel_Instances, null);
		panel_Instances.setLayout(null);
		
		JLabel lblNewLabel_2 = new JLabel("Positive Examples");
		lblNewLabel_2.setBounds(10, 11, 154, 14);
		panel_Instances.add(lblNewLabel_2);
		
		JScrollPane scrollPane = new JScrollPane();
		scrollPane.setBounds(10, 30, 600, 494);
		panel_Instances.add(scrollPane);
		
		textPanePos = new JTextPane();
		scrollPane.setViewportView(textPanePos);
		
		JScrollPane scrollPane_1 = new JScrollPane();
		scrollPane_1.setBounds(629, 30, 600, 494);
		panel_Instances.add(scrollPane_1);
		
		textPaneNeg = new JTextPane();
		scrollPane_1.setViewportView(textPaneNeg);
		
		JLabel lblNegativeExamples = new JLabel("Negative Examples");
		lblNegativeExamples.setBounds(629, 11, 154, 14);
		panel_Instances.add(lblNegativeExamples);
		
		JPanel panel_dataAnalisis = new JPanel();
		tabbedPaneMain.addTab("Data Analisis", null, panel_dataAnalisis, null);
		panel_dataAnalisis.setLayout(null);
		
		scrollPane_Atts = new JScrollPane();
		scrollPane_Atts.setBounds(10, 29, 324, 495);
		panel_dataAnalisis.add(scrollPane_Atts);
		
		
		table_atts = new JTable(modelAtts);
		modelAtts.addColumn("Attribute id");
		table_atts.setAutoCreateRowSorter(true);
		scrollPane_Atts.setViewportView(table_atts);
		
		scrollPane_SelectAtt = new JScrollPane();
		scrollPane_SelectAtt.setBounds(344, 29, 752, 495);
		panel_dataAnalisis.add(scrollPane_SelectAtt);
		
		table_SelectAtt = new JTable(modelSelected);
		modelSelected.addColumn("Statistic");
		modelSelected.addColumn("Value");
		scrollPane_SelectAtt.setViewportView(table_SelectAtt);
		
		JLabel lblAttributes = new JLabel("Attributes");
		lblAttributes.setBounds(10, 11, 129, 14);
		panel_dataAnalisis.add(lblAttributes);
		
		JLabel lblSelectedAttribute = new JLabel("Selected Attribute");
		lblSelectedAttribute.setBounds(345, 11, 147, 14);
		panel_dataAnalisis.add(lblSelectedAttribute);
		
		JButton btnNewButton = new JButton("Generate Statistics");
		btnNewButton.setBounds(1106, 26, 123, 61);
		panel_dataAnalisis.add(btnNewButton);
		
		JButton btnRun = new JButton("Run");
		btnRun.setBounds(1217, 10, 157, 61);
		contentPane.add(btnRun);
		btnRun.setForeground(new Color(102, 153, 0));
		btnRun.setIcon(new ImageIcon("ico\\arrow_down.png"));
		btnRun.setFont(new Font("Tahoma", Font.BOLD, 12));
		
		lblPolicyS = new JLabel("Policy Saved As:");
		lblPolicyS.setBounds(801, 11, 90, 14);
		contentPane.add(lblPolicyS);
		
		lblLabelPcyLoaded = new JLabel("None");
		lblLabelPcyLoaded.setBounds(901, 10, 99, 14);
		contentPane.add(lblLabelPcyLoaded);
		
		JLabel lblPolicyU = new JLabel("Policy Used:");
		lblPolicyU.setBounds(801, 34, 90, 14);
		contentPane.add(lblPolicyU);
		
		lblPcySaved = new JLabel("None");
		lblPcySaved.setBounds(901, 34, 113, 14);
		contentPane.add(lblPcySaved);
		
		btnRunScript = new JButton("Script");
		btnRunScript.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent arg0) {
				
				try {
					//RunAction();
					RunActionMultiple_load_specific();
				} catch (Exception e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				}
				
			}
		});
		btnRunScript.setForeground(new Color(107, 142, 35));
		btnRunScript.setFont(new Font("Tahoma", Font.BOLD, 11));
		btnRunScript.setBounds(1060, 10, 157, 61);
		contentPane.add(btnRunScript);
		
	
		btnRun.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent arg0) {
				
				
					try {
						RunAction();
						//RunActionMultiple_load_specific();
					} catch (Exception e) {
						// TODO Auto-generated catch block
						e.printStackTrace();
					}
				
			}
		});
		String	listData[] =
			{
				"atom2var",
				"otherrule 2",
				"otherrule 3",
				"otherrule 4"
			};
	}
	private static void addPopup(Component component, final JPopupMenu popup) {
		component.addMouseListener(new MouseAdapter() {
			public void mousePressed(MouseEvent e) {
				if (e.isPopupTrigger()) {
					showMenu(e);
				}
			}
			public void mouseReleased(MouseEvent e) {
				if (e.isPopupTrigger()) {
					showMenu(e);
				}
			}
			private void showMenu(MouseEvent e) {
				popup.show(e.getComponent(), e.getX(), e.getY());
			}
		});
	}
	

     ///////////////////////////
     //////// ACTIONS //////////
     ///////////////////////////	
	
		public void RunAction() throws Exception {
			
			
			//Borrado Rows
			
			//borrarRows(model);
			//borrarRows(modelOp);
			//borrarRows(modelRules);
			model.setRowCount(0);
			modelOp.setRowCount(0);
			modelRules.setRowCount(0);
			
			
			System.out.println("RUN PRESIONADO");
			
			InetAddress localHost = InetAddress.getLocalHost();
			String hostname=localHost.getHostName();	
			
			String peer = "metaflip_node@"+hostname;
			String cookie = "erlang";
			String javaClient ="java";
			Integer programs = 0;
			if (this.chckbxPrograms.isSelected() == true)
				programs=1;
			System.out.println("checked: "+programs+"\n");
			  
				try {
				
				 long tiempoInicio = System.currentTimeMillis();
				 
				 OtpConnection conn;
				 OtpSelf self = new OtpSelf(javaClient, cookie.trim());
			     OtpPeer other = new OtpPeer(peer.trim()); 
			     conn = self.connect(other);
			     //OtpErlangObject[] args = new OtpErlangObject[]{ new OtpErlangString("C:/Users/Nando/workspace/IFPER/Instances/"+PosEx.getName()), new OtpErlangString("C:/Users/Nando/workspace/IFPER/Instances/"+NegEx.getName()),new OtpErlangLong(steps)};		
			   
			     OtpErlangObject[] args = new OtpErlangObject[]{ new OtpErlangString(PosEx.getAbsolutePath()), new OtpErlangString(NegEx.getAbsolutePath()), new OtpErlangLong(steps),new OtpErlangString(Operators.getAbsolutePath()), new OtpErlangLong(programs), new OtpErlangString(PcyLoadPath),new OtpErlangString(PcySavePath)};		
			     conn.sendRPC("main","main",args);
			     //conn.sendRPC("main","main",new OtpErlangList());
			     OtpErlangObject received = conn.receiveRPC(); 
			     
			     long totalTiempo = System.currentTimeMillis() - tiempoInicio;
			     double seg=totalTiempo*0.001;
			     lblTimeElapsed.setText("Elapsed Time: "+totalTiempo+" miliseg ("+seg+" s)");
			     System.out.println("El tiempo de demora es :" + totalTiempo + " miliseg");
			    
			     System.out.println("Connection Established with "+peer+"\n");
			     System.out.println("Recibido: "+received+"\n");
			     String FileRules= ((OtpErlangList)received).elementAt(0).toString();
				 String FileActs= ((OtpErlangList)received).elementAt(1).toString();
				 String FileProgrmas = ((OtpErlangList)received).elementAt(2).toString();
				 //String PathRules="C:\\Users\\Nando\\workspace\\IFPER\\src\\"+((OtpErlangList)received).elementAt(0).toString().substring(1, ((OtpErlangList)received).elementAt(0).toString().length()-1);
				 //String PathActs="C:\\Users\\Nando\\workspace\\IFPER\\src\\"+((OtpErlangList)received).elementAt(1).toString().substring(1, ((OtpErlangList)received).elementAt(1).toString().length()-1);
				 String PathRules=((OtpErlangList)received).elementAt(0).toString().substring(1, ((OtpErlangList)received).elementAt(0).toString().length()-1);
				 String PathActs=((OtpErlangList)received).elementAt(1).toString().substring(1, ((OtpErlangList)received).elementAt(1).toString().length()-1);
				 String PathPrograms=((OtpErlangList)received).elementAt(2).toString().substring(1, ((OtpErlangList)received).elementAt(2).toString().length()-1);
				 
				 System.out.println(PathRules);
				 
				 System.out.println(PathActs);
				 
				 FileReader fr = new FileReader(PathRules);
				 BufferedReader bf = new BufferedReader(fr);
				 String sCadena ="";
				 String Rules="";
				 
				 while ((sCadena = bf.readLine())!=null) {
					 String [] Datos=sCadena.split("//");
					 modelRules.addRow(new Object[]{Datos[0],Datos[1],Datos[2],Datos[3],Datos[4],Datos[5],Datos[6],Datos[7],Datos[14],Datos[15],Datos[16]});
					 
					 
					 }
				
			     			     
			     FileReader fr2 = new FileReader(PathActs);
				 BufferedReader bf2 = new BufferedReader(fr2);
				 String sCadena2 ="";
				 String Acts="";
				
				 while ((sCadena2 = bf2.readLine())!=null) {
					 String [] Datos=sCadena2.split(";");
					 modelOp.addRow(new Object[]{Datos[0],Datos[1],Datos[2],Datos[3]});
					 
					
					 }
				
			
				 
				 
				 FileReader fr3 = new FileReader(PathPrograms);
				 BufferedReader bf3 = new BufferedReader(fr3);
				 String sCadena3 ="";
				 String Progs="";
				 
				 int Row=0;
				 while ((sCadena3 = bf3.readLine())!=null) {
					 String [] Datos=sCadena3.split(";");
					 model.addRow(new Object[]{Datos[0],Datos[1],Datos[2],Datos[3],Datos[4],Datos[5],Datos[6]});
					 //table_programs.setValueAt(Datos[0], Row, 0);
					// table_programs.setValueAt(Datos[1], Row, 1);
					// table_programs.setValueAt(Datos[2], Row, 2);
					// table_programs.setValueAt(Datos[3], Row, 3);
					// Row+=1;
					 }
				 
				
				
				 //textPane_Programs.setText(Progs);
				 
				 System.out.println("Disconnecting....");
				   if(conn != null){
				     conn.close();
				   }
				   System.out.println("Successfuly Disconnected");
			     
			   }
			   catch (Exception exp) {
			     System.out.println("connection error is :" + exp.toString());
			     exp.printStackTrace();
			   }
			   
			   
		}
		
public void RunActionMultiple_load() throws Exception {
			
			
	
			
			System.out.println("RUN MULTIPLE");
			
			
			InetAddress localHost = InetAddress.getLocalHost();
			String hostname=localHost.getHostName();	
			
			String peer = "metaflip_node@"+hostname;
			String cookie = "erlang";
			String javaClient ="java";
			Integer programs = 0;
			if (this.chckbxPrograms.isSelected() == true)
				programs=1;
			System.out.println("checked: "+programs+"\n");
			
			long tiempoInicio = System.currentTimeMillis();
			for(int i=0; i<MultiplePEX.length; i++ ){
				try {
				
				
				 
				 OtpConnection conn;
				 OtpSelf self = new OtpSelf(javaClient, cookie.trim());
			     OtpPeer other = new OtpPeer(peer.trim()); 
			     conn = self.connect(other);
			     //OtpErlangObject[] args = new OtpErlangObject[]{ new OtpErlangString("C:/Users/Nando/workspace/IFPER/Instances/"+PosEx.getName()), new OtpErlangString("C:/Users/Nando/workspace/IFPER/Instances/"+NegEx.getName()),new OtpErlangLong(steps)};		
			   
			     OtpErlangObject[] args = new OtpErlangObject[]{ new OtpErlangString(MultiplePEX[i].getAbsolutePath()), new OtpErlangString(MultipleNEX[i].getAbsolutePath()), new OtpErlangLong(steps),new OtpErlangString(Operators.getAbsolutePath()), new OtpErlangLong(programs), new OtpErlangString(MultiplePCY[i].getAbsolutePath()),new OtpErlangString(MultipleNEX[i].getName()),new OtpErlangString(MultipleNEX[i].getName()),new OtpErlangString(MultiplePEX[i].getName())};		
			     conn.sendRPC("main","main_load",args);
			     //conn.sendRPC("main","main",new OtpErlangList());
			     OtpErlangObject received = conn.receiveRPC(); 
			     
			    
			    
			    
			     System.out.println("Connection Established with "+peer+"\n");
			     System.out.println("Recibido: "+received+"\n");
			     String FileRules= ((OtpErlangList)received).elementAt(0).toString();
				 String FileActs= ((OtpErlangList)received).elementAt(1).toString();
				 String FileProgrmas = ((OtpErlangList)received).elementAt(2).toString();
				 //String PathRules="C:\\Users\\Nando\\workspace\\IFPER\\src\\"+((OtpErlangList)received).elementAt(0).toString().substring(1, ((OtpErlangList)received).elementAt(0).toString().length()-1);
				 //String PathActs="C:\\Users\\Nando\\workspace\\IFPER\\src\\"+((OtpErlangList)received).elementAt(1).toString().substring(1, ((OtpErlangList)received).elementAt(1).toString().length()-1);
				 String PathRules=((OtpErlangList)received).elementAt(0).toString().substring(1, ((OtpErlangList)received).elementAt(0).toString().length()-1);
				 String PathActs=((OtpErlangList)received).elementAt(1).toString().substring(1, ((OtpErlangList)received).elementAt(1).toString().length()-1);
				 String PathPrograms=((OtpErlangList)received).elementAt(2).toString().substring(1, ((OtpErlangList)received).elementAt(2).toString().length()-1);
				 
				 System.out.println(PathRules);
				 System.out.println(PathActs);
				 
				 
				 //textPane_Programs.setText(Progs);
				 
				 System.out.println("Disconnecting....");
				   if(conn != null){
				     conn.close();
				   }
				   System.out.println("Successfuly Disconnected");
			     
			   }
			   catch (Exception exp) {
			     System.out.println("connection error is :" + exp.toString());
			     exp.printStackTrace();
			   }
			}//FOR	
			 long totalTiempo = System.currentTimeMillis() - tiempoInicio;
			 double seg=totalTiempo*0.001;
		     lblTimeElapsed.setText("Elapsed Time: "+totalTiempo+" miliseg ("+seg+" s)");
		     System.out.println("El tiempo de demora es :" + totalTiempo + " miliseg");
			   
		}
		


public void RunActionMultiple_save() throws Exception {
	
	FileWriter fichero = null;
    PrintWriter pw = null;
	fichero = new FileWriter("../IFPER/Solution/Summary_SavePcy.txt");
    pw = new PrintWriter(fichero);
	
	
	
	System.out.println("RUN MULTIPLE");
	
	
	InetAddress localHost = InetAddress.getLocalHost();
	String hostname=localHost.getHostName();	
	
	String peer = "metaflip_node@"+hostname;
	String cookie = "erlang";
	String javaClient ="java";
	Integer programs = 0;
	if (this.chckbxPrograms.isSelected() == true)
		programs=1;
	System.out.println("checked: "+programs+"\n");
	
	long tiempoInicio = System.currentTimeMillis();
	
	
	for(int i=0; i<MultiplePEX.length; i++ ){
		try {
		
		
		 
		 OtpConnection conn;
		 OtpSelf self = new OtpSelf(javaClient, cookie.trim());
	     OtpPeer other = new OtpPeer(peer.trim()); 
	     conn = self.connect(other);
	     //OtpErlangObject[] args = new OtpErlangObject[]{ new OtpErlangString("C:/Users/Nando/workspace/IFPER/Instances/"+PosEx.getName()), new OtpErlangString("C:/Users/Nando/workspace/IFPER/Instances/"+NegEx.getName()),new OtpErlangLong(steps)};		
	   
	     //PCY
	     String PCY=MultiplePEX[i].getAbsolutePath() + ".pcy";
	    
	     System.out.println("Saving PCY... "+PCY+"\n");
	     
	     OtpErlangObject[] args = new OtpErlangObject[]{ new OtpErlangString(MultiplePEX[i].getAbsolutePath()), 
	    		 new OtpErlangString(MultipleNEX[i].getAbsolutePath()), new OtpErlangLong(steps),
	    		 new OtpErlangString(Operators.getAbsolutePath()), new OtpErlangLong(programs), 
	    		 new OtpErlangString(PCY),new OtpErlangString(MultiplePEX[i].getName()),
	    		 new OtpErlangString(MultiplePEX[i].getName()),new OtpErlangString(MultiplePEX[i].getName())};		
	     conn.sendRPC("main","main_save",args);
	     //conn.sendRPC("main","main",new OtpErlangList());
	     OtpErlangObject received = conn.receiveRPC(); 
	     
	    
	    
	    
	     System.out.println("Connection Established with "+peer+"\n");
	     System.out.println("Recibido: "+received+"\n");
	     String FileRules= ((OtpErlangList)received).elementAt(0).toString();
		 String FileActs= ((OtpErlangList)received).elementAt(1).toString();
		 String FileProgrmas = ((OtpErlangList)received).elementAt(2).toString();
		 //String PathRules="C:\\Users\\Nando\\workspace\\IFPER\\src\\"+((OtpErlangList)received).elementAt(0).toString().substring(1, ((OtpErlangList)received).elementAt(0).toString().length()-1);
		 //String PathActs="C:\\Users\\Nando\\workspace\\IFPER\\src\\"+((OtpErlangList)received).elementAt(1).toString().substring(1, ((OtpErlangList)received).elementAt(1).toString().length()-1);
		 String PathRules=((OtpErlangList)received).elementAt(0).toString().substring(1, ((OtpErlangList)received).elementAt(0).toString().length()-1);
		 String PathActs=((OtpErlangList)received).elementAt(1).toString().substring(1, ((OtpErlangList)received).elementAt(1).toString().length()-1);
		 String PathPrograms=((OtpErlangList)received).elementAt(2).toString().substring(1, ((OtpErlangList)received).elementAt(2).toString().length()-1);
		 String BestRule=  ((OtpErlangList)received).elementAt(3).toString();
		 String Step = ((OtpErlangList)received).elementAt(4).toString();
		 
			String BestProgram=  ((OtpErlangList)received).elementAt(5).toString();
			String StepP = ((OtpErlangList)received).elementAt(6).toString();
		
		 System.out.println(PathRules);
		 System.out.println(PathActs);
		 
		 pw.println(MultiplePEX[i].getName()+"-> Rule: "+BestRule+"Step: "+Step);
		 
		 
		 //textPane_Programs.setText(Progs);
		 
		 System.out.println("Disconnecting....");
		   if(conn != null){
		     conn.close();
		   }
		   System.out.println("Successfuly Disconnected");
	     
	   }
	   catch (Exception exp) {
	     System.out.println("connection error is :" + exp.toString());
	     exp.printStackTrace();
	   }
	}//FOR	
	
	if (null != fichero)
        fichero.close();
	 long totalTiempo = System.currentTimeMillis() - tiempoInicio;
	 double seg=totalTiempo*0.001;
     lblTimeElapsed.setText("Elapsed Time: "+totalTiempo+" miliseg ("+seg+" s)");
     System.out.println("El tiempo de demora es :" + totalTiempo + " miliseg");
	   
}

public void Aux_save_pcy(int Op) throws Exception {

	
	FileWriter fichero = null;
    PrintWriter pwS = null;
	fichero = new FileWriter("../IFPER/Solution/Summary_SavePcy_oper_"+Op+".txt");
    pwS = new PrintWriter(fichero);
	
	
	
	System.out.println("RUN AUX SAVE PCY");
	
	
	InetAddress localHost = InetAddress.getLocalHost();
	String hostname=localHost.getHostName();	
	
	String peer = "metaflip_node@"+hostname;
	String cookie = "erlang";
	String javaClient ="java";
	Integer programs = 0;
	if (this.chckbxPrograms.isSelected() == true)
		programs=1;
	System.out.println("checked: "+programs+"\n");
	
	long tiempoInicio = System.currentTimeMillis();
	
	
	for(int Sample=1; Sample<=5; Sample++ ){		
			
			
			for(int Problem=1; Problem<=5; Problem++ ){
					
				
					try {
		
	 
				OtpConnection conn;
				OtpSelf self = new OtpSelf(javaClient, cookie.trim());
				OtpPeer other = new OtpPeer(peer.trim()); 
				conn = self.connect(other);
				//OtpErlangObject[] args = new OtpErlangObject[]{ new OtpErlangString("C:/Users/Nando/workspace/IFPER/Instances/"+PosEx.getName()), new OtpErlangString("C:/Users/Nando/workspace/IFPER/Instances/"+NegEx.getName()),new OtpErlangLong(steps)};		
   
				String Pex="../Multiple/sust_"+Problem+"_sample_"+Sample+"A.pex";
				String Nex="../Multiple/sust_"+Problem+"_sample_"+Sample+"A.nex";
				//String Pcy="../Multiple/sust_"+PCY+"_sample_"+Sample+".pex - copia ("+Problem+").pcy";
				String Pcy="../Multiple/sust_"+Problem+"_sample_"+Sample+"_Oper_"+Op+".pcy";
				String Opers="../Multiple/opers_Op_"+Op+".erl";
				
				//String OutRules= "SavePCY_Sample_"+Sample+"sust_"+Problem+"_oper_"+Op+".csv";
				//String OutActs= "SavePCY_Sample_"+Sample+"sust_"+Problem+"_oper_"+Op+".csv";
				//String OutProbs= "SavePCY_Sample_"+Sample+"sust_"+Problem+"_oper_"+Op+".csv";
				
				
				
				String OutRules= "SavePCY_Oper_"+Op+"_Sample_"+Sample+"_Problem_"+Problem+".csv";
				String OutActs= "SavePCY_Oper_"+Op+"_Sample_"+Sample+"_Problem_"+Problem+".csv";
				String OutProbs= "SavePCY_Oper_"+Op+"_Sample_"+Sample+"_Problem_"+Problem+".csv";
				
				
				
				System.out.println("PEX: "+Pex+"\n");
				System.out.println("NEX: "+Nex+"\n");	
				System.out.println("PCY: "+Pcy+"\n");
				System.out.println("Opers: "+Opers+"\n");
				System.out.println("OutRules: "+OutRules+"\n");	
				System.out.println("OutActs: "+OutActs+"\n");	
				System.out.println("OutProbs: "+OutProbs+"\n");	
				
				OtpErlangObject[] args = new OtpErlangObject[]{ new OtpErlangString(Pex),
						new OtpErlangString(Nex), new OtpErlangLong(1000),
						new OtpErlangString(Opers), new OtpErlangLong(programs),
						new OtpErlangString(Pcy),new OtpErlangString(OutRules),
						new OtpErlangString(OutActs),new OtpErlangString(OutProbs)};	
				
				conn.sendRPC("main","main_save",args);
				//conn.sendRPC("main","main",new OtpErlangList());
				OtpErlangObject received = conn.receiveRPC(); 
     
				
    
    
				System.out.println("Connection Established with "+peer+"\n");
				System.out.println("Recibido: "+received+"\n");
				String FileRules= ((OtpErlangList)received).elementAt(0).toString();
				String FileActs= ((OtpErlangList)received).elementAt(1).toString();
				String FileProgrmas = ((OtpErlangList)received).elementAt(2).toString();
				String BestRule=  ((OtpErlangList)received).elementAt(3).toString();
				String Step = ((OtpErlangList)received).elementAt(4).toString();
				String BestProgram=  ((OtpErlangList)received).elementAt(5).toString();
				String StepP = ((OtpErlangList)received).elementAt(6).toString();
				
				
				
				pwS.println(Pex+"-> Rule: "+BestRule+" Step: "+Step);
				
				//String PathRules="C:\\Users\\Nando\\workspace\\IFPER\\src\\"+((OtpErlangList)received).elementAt(0).toString().substring(1, ((OtpErlangList)received).elementAt(0).toString().length()-1);
				//String PathActs="C:\\Users\\Nando\\workspace\\IFPER\\src\\"+((OtpErlangList)received).elementAt(1).toString().substring(1, ((OtpErlangList)received).elementAt(1).toString().length()-1);
				String PathRules=((OtpErlangList)received).elementAt(0).toString().substring(1, ((OtpErlangList)received).elementAt(0).toString().length()-1);
				String PathActs=((OtpErlangList)received).elementAt(1).toString().substring(1, ((OtpErlangList)received).elementAt(1).toString().length()-1);
				String PathPrograms=((OtpErlangList)received).elementAt(2).toString().substring(1, ((OtpErlangList)received).elementAt(2).toString().length()-1);
	 
				
				System.out.println("PCY generada ../Multiple/sust_"+Problem+"_sample_"+Sample+"_Oper_"+Op+".pcy");
				System.out.println(PathRules);
				System.out.println(PathActs);
	 
				
				//textPane_Programs.setText(Progs);
	 
				System.out.println("Disconnecting....");
				if(conn != null){
					conn.close();
				}
				System.out.println("Successfuly Disconnected");
	   
				copy("../IFPER/Multiple/sust_"+Problem+"_sample_"+Sample+"_Oper_"+Op+".pcy","../IFPER/Multiple/sust_"+Problem+"_sample_"+Sample+"_Oper_"+Op+"_copia_1.pcy");
				copy("../IFPER/Multiple/sust_"+Problem+"_sample_"+Sample+"_Oper_"+Op+".pcy","../IFPER/Multiple/sust_"+Problem+"_sample_"+Sample+"_Oper_"+Op+"_copia_2.pcy");
				copy("../IFPER/Multiple/sust_"+Problem+"_sample_"+Sample+"_Oper_"+Op+".pcy","../IFPER/Multiple/sust_"+Problem+"_sample_"+Sample+"_Oper_"+Op+"_copia_3.pcy");
				copy("../IFPER/Multiple/sust_"+Problem+"_sample_"+Sample+"_Oper_"+Op+".pcy","../IFPER/Multiple/sust_"+Problem+"_sample_"+Sample+"_Oper_"+Op+"_copia_4.pcy");
				copy("../IFPER/Multiple/sust_"+Problem+"_sample_"+Sample+"_Oper_"+Op+".pcy","../IFPER/Multiple/sust_"+Problem+"_sample_"+Sample+"_Oper_"+Op+"_copia_5.pcy");
				}
					
				catch (Exception exp) {
				System.out.println("connection error is :" + exp.toString());
				exp.printStackTrace();
				}
			}//IF
			
			//FOR
			
			
		}
	
	if (null != fichero)
        fichero.close();
	 long totalTiempo = System.currentTimeMillis() - tiempoInicio;
	 double seg=totalTiempo*0.001;
     //lblTimeElapsed.setText("Elapsed Time: "+totalTiempo+" miliseg ("+seg+" s)");
     System.out.println("El tiempo de demora es :" + totalTiempo + " miliseg");
	   
}



public void RunActionMultiple_load_specific() throws Exception {
	
	FileWriter fichero = null;
    PrintWriter pw = null;
    FileWriter fichero2 = null;
    PrintWriter pw2 = null;
	
	//pw.println("Operators,Sample,PCY,Problem,BestRule,Step");
	
	
	System.out.println("RUN MULTIPLE");
	
	
	InetAddress localHost = InetAddress.getLocalHost();
	String hostname=localHost.getHostName();	
	
	String peer = "metaflip_node@"+hostname;
	String cookie = "erlang";
	String javaClient ="java";
	Integer programs = 0;
	if (this.chckbxPrograms.isSelected() == true)
		programs=1;
	System.out.println("checked: "+programs+"\n");
	
	long tiempoInicio = System.currentTimeMillis();
	
	for (int Op=1; Op<=5; Op++ ){		
		
		fichero = new FileWriter("../IFPER/Solution/Summary_oper"+Op+".txt");
	    pw = new PrintWriter(fichero);
		pw.println("Operators,Sample,PCY,Problem,BestRule,Step");
		
		
	    
		fichero2 = new FileWriter("../IFPER/Solution/SummaryMatrix_oper"+Op+".txt");
	    pw2 = new PrintWriter(fichero2);
	    
		Aux_save_pcy(Op);
		
		for(int Sample=1; Sample<=5; Sample++ ){
		
			for(int PCY=1; PCY<=5; PCY++ ){
				int pcy_copia=0;
				pw2.println(" ");
				for(int Problem=1; Problem<=5; Problem++ ){
						
					//if (Problem != PCY){
						pcy_copia++;
						try {
			
								
					OtpConnection conn;
					OtpSelf self = new OtpSelf(javaClient, cookie.trim());
					OtpPeer other = new OtpPeer(peer.trim()); 
					conn = self.connect(other);
					//OtpErlangObject[] args = new OtpErlangObject[]{ new OtpErlangString("C:/Users/Nando/workspace/IFPER/Instances/"+PosEx.getName()), new OtpErlangString("C:/Users/Nando/workspace/IFPER/Instances/"+NegEx.getName()),new OtpErlangLong(steps)};		
	   
					String Pex="../Multiple/sust_"+Problem+"_sample_"+Sample+"B.pex";
					String Nex="../Multiple/sust_"+Problem+"_sample_"+Sample+"B.nex";
					//String Pcy="../Multiple/sust_"+PCY+"_sample_"+Sample+".pex - copia ("+Problem+").pcy";
					String Pcy="../Multiple/sust_"+PCY+"_sample_"+Sample+"_Oper_"+Op+"_copia_"+pcy_copia+".pcy";
					String Opers="../Multiple/opers_Op_"+Op+".erl";
					
					String OutRules= "../Solution/Rules_Oper_"+Op+"Sample_"+Sample+"_Pcy_"+PCY+"sust_"+Problem+".csv";
					String OutActs= "../Solution/Acts_Oper_"+Op+"Sample_"+Sample+"_Pcy_"+PCY+"sust_"+Problem+".csv";
					String OutProbs= "../Solution/Progs_Oper_"+Op+"Sample_"+Sample+"_Pcy_"+PCY+"sust_"+Problem+".csv";
					
					
					System.out.println("PEX: "+Pex+"\n");
					System.out.println("NEX: "+Nex+"\n");	
					System.out.println("PCY: "+Pcy+"\n");
					System.out.println("Opers: "+Opers+"\n");
					System.out.println("OutRules: "+OutRules+"\n");	
					System.out.println("OutActs: "+OutActs+"\n");	
					System.out.println("OutProbs: "+OutProbs+"\n");	
					
					OtpErlangObject[] args = new OtpErlangObject[]{ new OtpErlangString(Pex),
							new OtpErlangString(Nex), new OtpErlangLong(1000),
							new OtpErlangString(Opers), new OtpErlangLong(programs),
							new OtpErlangString(Pcy),new OtpErlangString(OutRules),
							new OtpErlangString(OutActs),new OtpErlangString(OutProbs)};	
					
					conn.sendRPC("main","main_load",args);
					//conn.sendRPC("main","main",new OtpErlangList());
					OtpErlangObject received = conn.receiveRPC(); 
	     
					
	    
	    
					System.out.println("Connection Established with "+peer+"\n");
					System.out.println("Recibido: "+received+"\n");
					String FileRules= ((OtpErlangList)received).elementAt(0).toString();
					String FileActs= ((OtpErlangList)received).elementAt(1).toString();
					String FileProgrmas = ((OtpErlangList)received).elementAt(2).toString();
					String BestRule=  ((OtpErlangList)received).elementAt(3).toString();
					String Step = ((OtpErlangList)received).elementAt(4).toString();
					String BestProgram=  ((OtpErlangList)received).elementAt(5).toString();
					String StepP = ((OtpErlangList)received).elementAt(6).toString();
					
					System.out.println("SAMPLE: "+Sample+" PCY: "+ PCY+" Problem: "+Problem+" --> Mejor Regla: "+BestRule+" Step:"+Step);
					
					pw.println(Sample+","+PCY+","+Problem+","+BestRule+","+Step);
					pw2.printf(BestRule+","+Step+",");
					
					//String PathRules="C:\\Users\\Nando\\workspace\\IFPER\\src\\"+((OtpErlangList)received).elementAt(0).toString().substring(1, ((OtpErlangList)received).elementAt(0).toString().length()-1);
					//String PathActs="C:\\Users\\Nando\\workspace\\IFPER\\src\\"+((OtpErlangList)received).elementAt(1).toString().substring(1, ((OtpErlangList)received).elementAt(1).toString().length()-1);
					String PathRules=((OtpErlangList)received).elementAt(0).toString().substring(1, ((OtpErlangList)received).elementAt(0).toString().length()-1);
					String PathActs=((OtpErlangList)received).elementAt(1).toString().substring(1, ((OtpErlangList)received).elementAt(1).toString().length()-1);
					String PathPrograms=((OtpErlangList)received).elementAt(2).toString().substring(1, ((OtpErlangList)received).elementAt(2).toString().length()-1);
		 
					System.out.println(PathRules);
					System.out.println(PathActs);
		 
					
					//textPane_Programs.setText(Progs);
		 
					System.out.println("Disconnecting....");
					if(conn != null){
						conn.close();
					}
					System.out.println("Successfuly Disconnected");
		   
					}
					catch (Exception exp) {
					System.out.println("connection error is :" + exp.toString());
					exp.printStackTrace();
					}
				//}//IF
				//else
				//{
				//	pw.println(Sample+","+PCY+","+Problem+",-1,-1");
				//	pw2.printf("0,0,");
				//}
				}//FOR
				
				
			}
		}
		if (null != fichero)
	         fichero.close();
		if (null != fichero2)
	        fichero2.close();
	}
	 
	
	
	 long totalTiempo = System.currentTimeMillis() - tiempoInicio;
	 double seg=totalTiempo*0.001;
     lblTimeElapsed.setText("Elapsed Time: "+totalTiempo+" miliseg ("+seg+" s)");
     System.out.println("El tiempo de demora es :" + totalTiempo + " miliseg");
	   
}


		
		void borrarRows(DefaultTableModel model){
			int count = model.getRowCount();
			System.out.println("Count: "+count);
			if (count > 0)
			{
				for(int i=1; i<count; i++){
				model.removeRow(i);
				}
			}
		}
		
		static final int BUFFER_SIZE = 2048;  
		static final byte[] buffer = new byte[BUFFER_SIZE];  
		  
		public static void copy(String from, String to) throws IOException{  
		   InputStream in = null;  
		   OutputStream out = null;   
		   int amountRead;  
		   try {  
		      in = new FileInputStream(from);  
		      out = new FileOutputStream(to);  
		      while (true) {  
		            amountRead = in.read(buffer);  
		            if (amountRead == -1)   
		               break;              
		            out.write(buffer, 0, amountRead);   
		         }  
		      }   
		    finally {  
		      if (in != null) {  
		         in.close();  
		      }  
		      if (out != null) {  
		         out.close();  
		      }  
		   }  
		}  
		
		
		
}
	
	

