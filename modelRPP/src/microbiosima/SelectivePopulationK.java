// This model is an extension of Qinglong Zeng's selection model.
// The address of the selected model is: https://github.com/qz28/microbiosima
package microbiosima;

import java.io.IOException;
import java.util.Arrays;

import utils.BinarySearch;
import utils.VectorAddition;
import utils.random.MathUtil;

public class SelectivePopulationK extends Population{
    
    private SelectiveIndividualK[] compositionOfIndividuals;
    private SelectiveSpeciesRegistry selSpReg;
    double hsCoef;

    private double averageHF;
    private double varianceHF;



    double[] fitnessList;
    double[] hostFitnessTotals;

    int[][] parentalMGeneFitness;
    int[][] parentalHGeneFitness;
    double[][] parentalMicrobeFitness;
    double[][] parentalHostFitness;
    double[][] parentalMicrobeFitnessK;
    double[][] parentalHostFitnessK;
    double[] parentalk;

	
    
    public SelectivePopulationK(int numberOfMicrobePerHost, double[] environment, int noOfIndividual, double environmentalFactor, double pooledOrFiexd, int numberOfSamples, int sampleReplicates0, SelectiveSpeciesRegistry ssr, double hostSelectionCoef,boolean HMS_or_TMS) throws IOException {
            super(numberOfMicrobePerHost, environment, noOfIndividual, environmentalFactor, pooledOrFiexd, numberOfSamples, sampleReplicates0);
            compositionOfIndividuals = new SelectiveIndividualK[numberOfIndividual];
            fitnessList=new double[numberOfIndividual];
            hostFitnessTotals=new double[numberOfIndividual];
            parentalMGeneFitness=new int[numberOfIndividual][ssr.numOfTolGenes];
            parentalHGeneFitness=new int[numberOfIndividual][ssr.numOfTolGenes];
            parentalMicrobeFitness=new double[numberOfIndividual][numberOfEnvironmentalSpecies];
            parentalMicrobeFitnessK=new double[numberOfIndividual][numberOfEnvironmentalSpecies];
            parentalHostFitness=new double[numberOfIndividual][numberOfEnvironmentalSpecies];
            parentalHostFitnessK=new double[numberOfIndividual][numberOfEnvironmentalSpecies];
            parentalk=new double[numberOfIndividual];
            selSpReg=ssr;
            hsCoef=hostSelectionCoef;
            averageHF=0;
            varianceHF=0;
            for (int i=0;i<numberOfIndividual;i++){
                compositionOfIndividuals[i]=new SelectiveIndividualK(multiNomialDist,numberOfMicrobePerHost,numberOfEnvironmentalSpecies,environment,ssr,HMS_or_TMS);
                fitnessList[i]=compositionOfIndividuals[i].getHostFitness(0);
            }           
        }



    @Override
    public void parentalInheritanceAndEnvironmentalAcquisition() {
        double runningTotals=0;
        for (int i=0;i<numberOfIndividual;i++){
            runningTotals+=Math.pow(hsCoef,fitnessList[i]);
            hostFitnessTotals[i]=runningTotals;
        }
        int[] oldAncestryIndex= new int[numberOfIndividual];
        System.arraycopy(ancestryIndex,0,oldAncestryIndex, 0,numberOfIndividual);
        for(int i=0;i<numberOfIndividual;i++){
            int index=BinarySearch.binarySearch(hostFitnessTotals, MathUtil.getNextFloat(runningTotals));
            parentalIndex[i]=index;
            ancestryIndex[i]=oldAncestryIndex[parentalIndex[i]];
            System.arraycopy(getIndividuals()[index].microbiome,
            0,parentalContribution[i], 0,numberOfEnvironmentalSpecies);
            System.arraycopy(getIndividuals()[index].microbeGenesFitness,
                                0,parentalMGeneFitness[i], 0,getSpeciesRegistry().numOfTolGenes);
            System.arraycopy(getIndividuals()[index].hostGenesFitness,
                                0,parentalHGeneFitness[i], 0,getSpeciesRegistry().numOfTolGenes);
            System.arraycopy(getIndividuals()[index].microbeFitnessRecords,
                                0,parentalMicrobeFitness[i], 0,numberOfEnvironmentalSpecies);
            System.arraycopy(getIndividuals()[index].microbeFitnessRecordsK,
                    0,parentalMicrobeFitnessK[i], 0,numberOfEnvironmentalSpecies);
            System.arraycopy(getIndividuals()[index].hostFitnessRecords,
                                0,parentalHostFitness[i], 0,numberOfEnvironmentalSpecies);
            System.arraycopy(getIndividuals()[index].hostFitnessRecordsK,
                    0,parentalHostFitnessK[i], 0,numberOfEnvironmentalSpecies);
            parentalk[i]=getIndividuals()[index].k;
        }
        
        
        VectorAddition.additionOfVectors(environmentalContribution, percentageOfpooledOrFixed, 1 - percentageOfpooledOrFixed, microbiomeSum,
                                         initialEnvironment);
        
        for (int i = 0; i < numberOfIndividual; i++) {
			VectorAddition.additionOfVectors(mixedContribution, coefficient,//(1 - environmentalFactor) / this.numberOfMicrobePerHost
					environmentalFactor, parentalContribution[i],
					environmentalContribution);
                        getSpeciesRegistry().getMFitnessSelection(mixedContribution,parentalMicrobeFitnessK[i]);
			multiNomialDist.updateProb(mixedContribution);
			multiNomialDist.multisample(getIndividuals()[i].microbiome, numberOfMicrobePerHost);
            System.arraycopy(parentalMGeneFitness[i], 0,getIndividuals()[i].microbeGenesFitness, 0,getSpeciesRegistry().numOfTolGenes);
            System.arraycopy(parentalHGeneFitness[i], 0,getIndividuals()[i].hostGenesFitness, 0,getSpeciesRegistry().numOfTolGenes);
            System.arraycopy(parentalMicrobeFitness[i], 0,getIndividuals()[i].microbeFitnessRecords, 0,numberOfEnvironmentalSpecies);
            System.arraycopy(parentalMicrobeFitnessK[i], 0,getIndividuals()[i].microbeFitnessRecordsK, 0,numberOfEnvironmentalSpecies);
            System.arraycopy(parentalHostFitness[i], 0,getIndividuals()[i].hostFitnessRecords, 0,numberOfEnvironmentalSpecies);
            System.arraycopy(parentalHostFitnessK[i], 0,getIndividuals()[i].hostFitnessRecordsK, 0,numberOfEnvironmentalSpecies);
            getIndividuals()[i].k=parentalk[i];
            fitnessList[i]=getSpeciesRegistry().getTotalFitness(getIndividuals()[i].getMicrobiome(), getIndividuals()[i].hostFitnessRecordsK);
        }
    }


    public double averageHostFitness(){
        double totalF=0;
        for(double fitness:fitnessList){
            totalF+=fitness;
        }
        averageHF=totalF/fitnessList.length;
        return averageHF;
    }

    public double varianceHostFitness(){
        double totalF2=0;
        for(double fitness:fitnessList){
            totalF2+=fitness*fitness;
        }
        varianceHF=totalF2/fitnessList.length-averageHF*averageHF;
        return varianceHF;
    }

    public double cosOfMH(){
        double total=0;
        for(SelectiveIndividualK host:getIndividuals()){
            total+=host.getCosTheta();
        }
        return total/numberOfIndividual;
    }

    public String printOutHFitness() {
        StringBuilder sb = new StringBuilder();
        for (int i = 0; i < numberOfIndividual; i++) {
            sb.append(Math.pow(hsCoef,fitnessList[i]-1)).append("\t");
        }
        return sb.toString().trim();
    }

    public String printOutMFitness() {
        StringBuilder sb = new StringBuilder();
        for (int i = 0; i < numberOfIndividual; i++) {
            sb.append(Math.pow(selSpReg.msc,selSpReg.getTotalFitness(getIndividuals()[i].getMicrobiome(), getIndividuals()[i].microbeFitnessRecords)-1)).append("\t");
        }
        return sb.toString().trim();
    }

    public String printBacteriaContents() {
        double[] bacteriaContents=new double[3];
        for (SelectiveIndividualK host:getIndividuals()){
            VectorAddition.additionOfVectors(bacteriaContents,1,1, host.goodVersusBad(),bacteriaContents);
        }
        StringBuilder sb= new StringBuilder();
        for (int i=0;i<3;i++){
            sb.append(bacteriaContents[i]).append("\t");
        }
        return sb.toString().trim();
    }
    public String printk() {
        double[] kop=new double[numberOfIndividual];
        for (int i = 0; i < numberOfIndividual; i++) {
            kop[i]=getIndividuals()[i].k;
        }
        StringBuilder sb= new StringBuilder();
        for (int i=0;i<numberOfIndividual;i++){
            sb.append(kop[i]).append("\t");
        }
        return sb.toString().trim();
    }

    @Override
    public SelectiveIndividualK[] getIndividuals(){
        return compositionOfIndividuals;
    }

    public SelectiveSpeciesRegistry getSpeciesRegistry(){
        return selSpReg;
    }
    

    
}
    
    
