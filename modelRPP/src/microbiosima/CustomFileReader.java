// This model is an extension of Qinglong Zeng's selection model.
// The address of the selected model is: https://github.com/qz28/microbiosima

package microbiosima;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

/*
 * @author John
 */
public class CustomFileReader {
	private String[] commands;
        public int numberOfLines;

	public CustomFileReader(String file_path, int number_of_line) throws IOException {
		BufferedReader bf = new BufferedReader(new FileReader(file_path));
		commands = new String[number_of_line];
                numberOfLines=number_of_line;
		int i = 0;
		while (i<number_of_line) {
			commands[i] = bf.readLine();
			i++;
		}
		bf.close();
	}
        
        public CustomFileReader(String file_path) throws IOException {
		BufferedReader bf = new BufferedReader(new FileReader(file_path));
		String aLine;
                List<String> commandList=new ArrayList<>();
		int i = 0;
		while ((aLine = bf.readLine()) != null) {
			commandList.add(aLine);
			i++;
		}
                commands = new String[i];
                commandList.toArray(commands);
                numberOfLines=i;
		bf.close();
	}
        
	/**
	 * @return the commands
	 */
	public String getCommand(int index) {
		return commands[index];
	}
	
	public String[] getCommandSplit(int index){
		return commands[index].split("\t");
	}
        
        public double[] getNumericArray(int index){
            String[] stringArray=getCommandSplit(index);
            double[] numericArray=new double[stringArray.length];
            for (int i=0;i<stringArray.length;i++){
                numericArray[i]=Double.parseDouble(stringArray[i]);
            }
            return numericArray;
        }
	
	

}
