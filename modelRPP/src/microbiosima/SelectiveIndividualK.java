// This model is an extension of Qinglong Zeng's selection model.
// The address of the selected model is: https://github.com/qz28/microbiosima


package microbiosima;

import utils.random.MathUtil;
import utils.random.Multinomial2;


public class SelectiveIndividualK extends Individual {

    private double hostFitness;
    private int numOfMGenes;
    int[] microbeGenesFitness;
    int[] hostGenesFitness;
    double[] microbeFitnessRecords;
    double[] hostFitnessRecords;
    double[] microbeFitnessRecordsK;
    double[] hostFitnessRecordsK;
    double k;

    public SelectiveIndividualK(Multinomial2 MultDist, int nomph, int noes,double[] environment , SelectiveSpeciesRegistry ssr, boolean HMS_or_TMS) {
        super(MultDist, nomph, noes, environment);
        numOfMGenes=ssr.numOfTolGenes;
        microbeGenesFitness=new int[numOfMGenes];
        hostGenesFitness=new int[numOfMGenes];
        microbeFitnessRecords=new double[noes];
        microbeFitnessRecordsK=new double[noes];
        hostFitnessRecords=new double[noes];
        hostFitnessRecordsK=new double[noes];
		if (HMS_or_TMS){
			for(int i=0;i<numOfMGenes;i++){
				microbeGenesFitness[i]=MathUtil.getNextInt(2)-1;
			}
		}else microbeGenesFitness=ssr.mfr;
		hostGenesFitness=ssr.hfr;
        k= MathUtil.getNextFloat(1);
        ssr.getFitness(microbeFitnessRecords,hostFitnessRecords, microbeGenesFitness,hostGenesFitness);
        hostProvideResources(microbeFitnessRecords,microbeFitnessRecordsK,hostFitnessRecords,hostFitnessRecordsK,k,noes);
        hostFitness=ssr.getTotalFitness(microbiome,hostFitnessRecordsK);
    }
    


    public void hostProvideResources(double[] microbeFitnessRecords, double[] microbeFitnessRecordsK,
                                     double[] hostFitnessRecords, double[] hostFitnessRecordsK, double k, int noes){
        for (int i = 0; i < noes; i++) {
            if (hostFitnessRecords[i]>0){
                microbeFitnessRecordsK[i]= microbeFitnessRecords[i]+hostFitnessRecords[i]*k;
                hostFitnessRecordsK[i]=hostFitnessRecords[i]*(1-k);
            }
            else if (hostFitnessRecords[i]<0){
                microbeFitnessRecordsK[i]= microbeFitnessRecords[i]+hostFitnessRecords[i]*k;
                hostFitnessRecordsK[i]=hostFitnessRecords[i]*(1+k);
            }
            else {
                microbeFitnessRecordsK[i] = microbeFitnessRecords[i];
                hostFitnessRecordsK[i] = hostFitnessRecords[i];
            }
        }
    }
    public double getHostFitness(double hostSelectionCoef){
        if(hostSelectionCoef==0)
            return hostFitness;
        else
            return Math.pow(hostSelectionCoef,hostFitness);
    }

    public double getCosTheta(){
        double innerProduct=0;
        double Mlength=0;
        double Hlength=0;
        for(int i=0;i<numOfMGenes;i++){
            innerProduct+=microbeGenesFitness[i]*hostGenesFitness[i];
            Mlength+=microbeGenesFitness[i]*microbeGenesFitness[i];
            Hlength+=hostGenesFitness[i]*hostGenesFitness[i];
        }
        return innerProduct/Math.sqrt(Hlength*Mlength);
    }

    public double[]  goodVersusBad(){
        double[] bacteriaContents=new double[3];
        for(int i=0;i<hostFitnessRecords.length;i++){
            if (hostFitnessRecords[i]<0)
                bacteriaContents[0]+=getMicrobiome()[i];
            else if (hostFitnessRecords[i]==0)
                bacteriaContents[1]+=getMicrobiome()[i];
            else
                bacteriaContents[2]+=getMicrobiome()[i];
        }
        return bacteriaContents;
    }

    public String printBacteriaContents(){
        StringBuilder sb=new StringBuilder();
        double[] bacteriaContents=goodVersusBad();
        for (int i=0;i<bacteriaContents.length;i++){
            sb.append(bacteriaContents[i]).append('\t');
        }
        return sb.toString().trim();
    }
    
}
