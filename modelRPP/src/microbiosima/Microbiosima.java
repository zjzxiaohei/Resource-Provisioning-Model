// This model is an extension of Qinglong Zeng's selection model.
// The address of the selected model is: https://github.com/qz28/microbiosima


package microbiosima;

import java.io.BufferedWriter;
import java.io.FileNotFoundException;
import java.io.FileWriter;
import java.io.IOException;
import java.io.PrintWriter;
import java.io.UnsupportedEncodingException;
import java.util.Arrays;

import org.apache.commons.cli.CommandLine;
import org.apache.commons.cli.CommandLineParser;
import org.apache.commons.cli.DefaultParser;
import org.apache.commons.cli.HelpFormatter;
import org.apache.commons.cli.Option;
import org.apache.commons.cli.Option.Builder;
import org.apache.commons.cli.OptionGroup;
import org.apache.commons.cli.Options;
import org.apache.commons.cli.ParseException;

/**
 *
 * @author John
 */
public class Microbiosima {

	private static final String VERSION = "1.0";

	/**
	 * @param args
	 *            the command line arguments
	 * @throws java.io.FileNotFoundException
	 * @throws java.io.UnsupportedEncodingException
	 */
	public static void main(String[] args) throws FileNotFoundException,
			UnsupportedEncodingException {
		//Init with default values
		int populationSize = 500;
		int microSize = 1000;
		int numberOfSpecies = 150;
		int numberOfGeneration = 10000;
		int numberOfObservation = 100;
		int numberOfReplication = 1;
		double pctEnv = 0;
		double pctPool = 0;


		Options options = new Options();

		Option help = new Option("h", "help", false, "print this message");
		Option version = new Option("v", "version", false,
				"print the version information and exit");
		options.addOption(help);
		options.addOption(version);
		
		options.addOption(Option.builder("o").longOpt("obs").hasArg()
				.argName("OBS").desc("Number generation for observation [default: 100]")
				.build());
		options.addOption(Option.builder("r").longOpt("rep").hasArg()
				.argName("REP").desc("Number of replication [default: 1]")
				.build());
					
		Builder C = Option.builder("c").longOpt("config")
				.numberOfArgs(4).argName("Pop Micro Spec Gen")
				.desc("Four Parameters in the following orders: "
						+ "(1) population size, (2) microbe size, (3) number of species, (4) number of generation"
						+ " [default: 500 1000 150 10000]");
		options.addOption(C.build());

		HelpFormatter formatter = new HelpFormatter();
		String syntax = "microbiosima pctEnv pctPool";
		String header = 
				"\nSimulates the evolutionary and ecological dynamics of microbiomes within a population of hosts.\n\n"+
"required arguments:\n"+
"  pctEnv             Percentage of environmental acquisition\n"+
"  pctPool            Percentage of pooled environmental component\n"
+ "\noptional arguments:\n";
		String footer = "\n";
		
		formatter.setWidth(80);

		
		CommandLineParser parser = new DefaultParser();
		CommandLine cmd = null;

		try {
			cmd = parser.parse(options, args);
			String[] pct_config = cmd.getArgs();
			
			if (cmd.hasOption("h") || args.length == 0) {
				formatter.printHelp(syntax, header, options, footer, true);
				System.exit(0);
			}
			if(cmd.hasOption("v")){
				System.out.println("Microbiosima "+VERSION);
				System.exit(0);
			}
			if (pct_config.length != 2){
				System.out
						.println("ERROR! Required exactly two argumennts for pct_env and pct_pool. It got "
								+ pct_config.length + ": " + Arrays.toString(pct_config));
				formatter.printHelp(syntax, header, options, footer, true);
				System.exit(3);
			}
			else{
				pctEnv = Double.parseDouble(pct_config[0]);
				pctPool = Double.parseDouble(pct_config[1]);
				if(pctEnv<0 || pctEnv >1){
					System.out.println(
						"ERROR: pctEnv (Percentage of environmental acquisition) must be between 0 and 1 (pctEnv="
						+ pctEnv + ")! EXIT");
					System.exit(3);
				}
				if(pctPool<0 || pctPool >1){
					System.out.println(
						"ERROR: pctPool (Percentage of pooled environmental component must) must be between 0 and 1 (pctPool="
						+ pctPool + ")! EXIT");
					System.exit(3);
				}
				
			}
			if (cmd.hasOption("config")){
				String[] configs = cmd.getOptionValues("config");
				populationSize = Integer.parseInt(configs[0]);
				microSize = Integer.parseInt(configs[1]);
				numberOfSpecies = Integer.parseInt(configs[2]);
				numberOfGeneration = Integer.parseInt(configs[3]);
			}
			if (cmd.hasOption("obs")){
				numberOfObservation= Integer.parseInt(cmd.getOptionValue("obs"));
			}
			if (cmd.hasOption("rep")){
				numberOfReplication= Integer.parseInt(cmd.getOptionValue("rep"));
			}			
			
			
			
		} catch (ParseException e) {
			e.printStackTrace();
			System.exit(3);
		}
		
		StringBuilder sb = new StringBuilder();
		sb.append("Configuration Summary:")
			.append("\n\tPopulation size: ").append(populationSize)
			.append("\n\tMicrobe size: ").append(microSize)
			.append("\n\tNumber of species: ").append(numberOfSpecies)
			.append("\n\tNumber of generation: ").append(numberOfGeneration)
			.append("\n\tNumber generation for observation: ").append(numberOfObservation)
			.append("\n\tNumber of replication: ").append(numberOfReplication)
			.append("\n");
		System.out.println(sb.toString());	
		
		
//		System.exit(3);
		// LogNormalDistribution lgd=new LogNormalDistribution(0,1);
		// environment=lgd.sample(150);
		// double environment_sum=0;
		// for (int i=0;i<No;i++){
		// environment_sum+=environment[i];
		// }
		// for (int i=0;i<No;i++){
		// environment[i]/=environment_sum;
		// }
		
		double[] environment = new double[numberOfSpecies];
		for (int i = 0; i < numberOfSpecies; i++) {
			environment[i] = 1 / (double) numberOfSpecies;
		}

		for (int rep = 0; rep < numberOfReplication; rep++) {
			String prefix = ""+(rep+1)+"_";
			String sufix = "_E" + pctEnv + "_P" + pctPool + ".txt";
		        
			System.out.println("Output 5 result files in the format of: "+prefix+"[****]" +sufix);    
			try {

				PrintWriter file1 = new PrintWriter(new BufferedWriter(
						new FileWriter(prefix + "gamma_diversity" + sufix)));
				PrintWriter file2 = new PrintWriter(new BufferedWriter(
						new FileWriter(prefix + "alpha_diversity" + sufix)));
				PrintWriter file3 = new PrintWriter(new BufferedWriter(
						new FileWriter(prefix + "beta_diversity" + sufix)));
				PrintWriter file4 = new PrintWriter(new BufferedWriter(
						new FileWriter(prefix + "sum" + sufix)));
				PrintWriter file5 = new PrintWriter(new BufferedWriter(
						new FileWriter(prefix + "inter_generation_distance" + sufix)));
				PrintWriter file6 = new PrintWriter(new BufferedWriter(
						new FileWriter(prefix
								+ "environment_population_distance" + sufix)));

				Population population = new Population(microSize, environment,
						populationSize, pctEnv, pctPool, 0, 0);
				while (population.getNumberOfGeneration() < numberOfGeneration) {
					population.sumSpecies();
					if (population.getNumberOfGeneration() % numberOfObservation == 0) {
						file1.println(population.gammaDiversity(true));
						file2.println(population.alphaDiversity(true));
						file3.print(population.betaDiversity(true));
						file3.print("\t");
						file3.println(population.BrayCurtis(true));
						file4.println(population.printOut());
						file5.println(population.interGenerationDistance());
						file6.println(population.environmentPopulationDistance());
					}
					population.getNextGen();
				}
				file1.close();
				file2.close();
				file3.close();
				file4.close();
				file5.close();
				file6.close();
			} catch (IOException e) {
				e.printStackTrace();
			}
		}
	}
	// TODO code application logic here
}
