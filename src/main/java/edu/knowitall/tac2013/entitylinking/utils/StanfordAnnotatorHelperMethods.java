package edu.knowitall.tac2013.entitylinking.utils;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.Stack;
import java.util.WeakHashMap;
import java.util.concurrent.ExecutorService;
import java.util.regex.Pattern;

import org.apache.commons.io.IOUtils;

import scala.actors.threadpool.Arrays;
import scala.actors.threadpool.ExecutionException;
import scala.actors.threadpool.Executors;
import scala.actors.threadpool.TimeUnit;
import scala.actors.threadpool.TimeoutException;

import edu.knowitall.collection.immutable.Interval;
import edu.stanford.nlp.dcoref.CorefChain;
import edu.stanford.nlp.dcoref.CoNLL2011DocumentReader.NamedEntityAnnotation;
import edu.stanford.nlp.dcoref.CorefChain.CorefMention;
import edu.stanford.nlp.dcoref.CorefCoreAnnotations.CorefChainAnnotation;
import edu.stanford.nlp.dcoref.CorefCoreAnnotations.CorefClusterIdAnnotation;
import edu.stanford.nlp.dcoref.CorefCoreAnnotations.CorefGraphAnnotation;
import edu.stanford.nlp.ling.CoreAnnotations.*;
import edu.stanford.nlp.ling.CoreLabel;
import edu.stanford.nlp.pipeline.Annotation;
import edu.stanford.nlp.pipeline.StanfordCoreNLP;
import edu.stanford.nlp.time.TimeAnnotations.TimexAnnotation;
import edu.stanford.nlp.time.Timex;
import edu.stanford.nlp.util.CoreMap;
import edu.stanford.nlp.util.IntTuple;
import edu.stanford.nlp.util.Pair;
import edu.knowitall.tac2013.entitylinking.SolrHelper;




public class StanfordAnnotatorHelperMethods {
	
	private final StanfordCoreNLP corefPipeline;
	private final StanfordCoreNLP regularPipeline;
	private String filePath = "/homes/gws/jgilme1/docs/";
	private Map<String,Annotation> corefAnnotationMap;
	
	
	public StanfordAnnotatorHelperMethods(){
		Properties corefProps = new Properties();
	    corefProps.put("annotators", "tokenize, cleanxml, ssplit, pos, lemma, ner, parse, dcoref");
	    corefProps.put("clean.allowflawedxml", "true");
	    corefProps.put("ner.useSUTime", "false");
	    //clean all xml tags
		this.corefPipeline = new StanfordCoreNLP(corefProps);
		
		corefAnnotationMap = new HashMap<String,Annotation>();
		
		
		Properties regularProps = new Properties();
		regularProps.put("annotators", "tokenize, cleanxml, ssplit, pos, lemma, ner");
		regularProps.put("clean.allowflawedxml","true");
		regularProps.put("ner.useSUTime", "false");
		this.regularPipeline = new StanfordCoreNLP(regularProps);
		
	}
	
	public void clearHashMaps(){
		corefAnnotationMap.clear();
	}

	
	public List<Interval> getCorefMentions(String xmlString, Integer begOffset) {
		Annotation document = new Annotation(xmlString);
		scala.actors.threadpool.ExecutorService executor = Executors.newSingleThreadExecutor();
		try{
		  executor.submit(new AnnotationRunnable(document,corefPipeline)).get(60, TimeUnit.SECONDS);
		}
		catch(Exception e){
			return new ArrayList<Interval>();
		}
		finally{
			executor.shutdown();
		}

		
		
		
		Map<Integer, CorefChain> graph = document.get(CorefChainAnnotation.class);
		Integer corefClusterID = null;
		List<CoreMap> sentences = document.get(SentencesAnnotation.class);
		
	    for(CoreMap sentence: sentences){
	    	for(CoreLabel token: sentence.get(TokensAnnotation.class)){
	    		if(token.beginPosition() == begOffset){
	    			corefClusterID = token.get(CorefClusterIdAnnotation.class);
	    		}
	    	}
	    }
	    
		
	    if(corefClusterID != null){
	    	List<Interval> offsets = new ArrayList<Interval>();
	    	for(CorefMention m : graph.get(corefClusterID).getMentionsInTextualOrder()){
	    		offsets.add(getCharIntervalFromCorefMention(document,m.sentNum,m.startIndex,m.endIndex));
	    	}
	    	return offsets;
	    }
	    else{
	    	return new ArrayList<Interval>();
	    }
		
	}
	
	public String getCorefRepresentativeString(String xmlString, Integer begOffset) {
		Annotation document = new Annotation(xmlString);
		scala.actors.threadpool.ExecutorService executor = Executors.newSingleThreadExecutor();
		try{
		  executor.submit(new AnnotationRunnable(document,corefPipeline)).get(60, TimeUnit.SECONDS);
		}
		catch(Exception e){
			return null;
		}
		finally{
			executor.shutdown();
		}

		
		
		
		Map<Integer, CorefChain> graph = document.get(CorefChainAnnotation.class);
		Integer corefClusterID = null;
		List<CoreMap> sentences = document.get(SentencesAnnotation.class);
		
	    for(CoreMap sentence: sentences){
	    	for(CoreLabel token: sentence.get(TokensAnnotation.class)){
	    		if(token.beginPosition() == begOffset){
	    			corefClusterID = token.get(CorefClusterIdAnnotation.class);
	    		}
	    	}
	    }
	    
		
	    if(corefClusterID != null){
	    	return graph.get(corefClusterID).getRepresentativeMention().mentionSpan;
	    }
	    else{
	    	return null;
	    }
		
	}
	
	public List<String> getCorefStringMentions(String xmlString, Integer begOffset) {
		Annotation document = new Annotation(xmlString);
		scala.actors.threadpool.ExecutorService executor = Executors.newSingleThreadExecutor();
		try{
		  executor.submit(new AnnotationRunnable(document,corefPipeline)).get(60, TimeUnit.SECONDS);
		}
		catch(Exception e){
			return new ArrayList<String>();
		}
		finally{
			executor.shutdown();
		}

		
		
		
		Map<Integer, CorefChain> graph = document.get(CorefChainAnnotation.class);
		Integer corefClusterID = null;
		List<CoreMap> sentences = document.get(SentencesAnnotation.class);
		
	    for(CoreMap sentence: sentences){
	    	for(CoreLabel token: sentence.get(TokensAnnotation.class)){
	    		if(token.beginPosition() == begOffset){
	    			corefClusterID = token.get(CorefClusterIdAnnotation.class);
	    		}
	    	}
	    }
	    
		
	    if(corefClusterID != null){
	    	List<String> mentions = new ArrayList<String>();
	    	for(CorefMention m : graph.get(corefClusterID).getMentionsInTextualOrder()){
	    		mentions.add(m.mentionSpan);
	    	}
	    	return mentions;
	    }
	    else{
	    	return new ArrayList<String>();
	    }
		
	}
	
	
	public List<String> getCorefTypes(String xmlString, Integer begOffset) {
		Annotation document = new Annotation(xmlString);
		scala.actors.threadpool.ExecutorService executor = Executors.newSingleThreadExecutor();
		try{
		  executor.submit(new AnnotationRunnable(document,regularPipeline)).get(60, TimeUnit.SECONDS);
		}
		catch(Exception e){
			return new ArrayList<String>();
		}
		finally{
			executor.shutdown();
		}

		List<CoreMap> sentences = document.get(SentencesAnnotation.class);
		String ne = "";
	    for(CoreMap sentence: sentences){
	    	for(CoreLabel token: sentence.get(TokensAnnotation.class)){
	    		if(token.beginPosition() == begOffset){
	    			ne = token.get(NamedEntityTagAnnotation.class);
	    		}
	    	}
	    }
	    if((!ne.equals("ORGANIZATION")) && (!ne.equals("PERSON")) && (!ne.equals("LOCATION"))){
	    	return new ArrayList<String>();
	    }
	    else{
	    	List<List<CoreLabel>> allTokens = new ArrayList<List<CoreLabel>>();
	    	List<CoreLabel> relevantTokens = new ArrayList<CoreLabel>();
	    	int sentIndex =0;
		    for(CoreMap sentence: sentences){
		    	List<CoreLabel> sentenceTokenList = new ArrayList<CoreLabel>();
		    	int tokenIndex =0;
		    	for(CoreLabel token: sentence.get(TokensAnnotation.class)){
		    			String net = token.get(NamedEntityTagAnnotation.class);
	    				token.setIndex(tokenIndex);
	    				token.setSentIndex(sentIndex);		    			
		    			if(net.equals(ne)){
		    				relevantTokens.add(token);
		    			}
		    			sentenceTokenList.add(token);
		    		tokenIndex +=1 ;
		    	}
		    	allTokens.add(sentenceTokenList);
		    	sentIndex += 1;
		    }
	    	if(!relevantTokens.isEmpty()){
	    		
		    	List<List<CoreLabel>> matchingTypes = new ArrayList<List<CoreLabel>>();
		    	List<CoreLabel> firstTokenList = new ArrayList<CoreLabel>();
		    	firstTokenList.add(relevantTokens.get(0));
		    	matchingTypes.add(firstTokenList);
		    	relevantTokens.remove(0);
		    	for(CoreLabel t : relevantTokens){
		    		int currIndex = matchingTypes.size()-1;
		    		List<CoreLabel> lastTokenList = matchingTypes.get(currIndex);
		    		CoreLabel lastToken = lastTokenList.get(lastTokenList.size()-1);
		    		
		    		if((t.sentIndex() == lastToken.sentIndex()) &&  (t.index() == (1 + lastToken.index()))){
		    			matchingTypes.get(currIndex).add(t);
		    		}
		    		else if((t.sentIndex()== lastToken.sentIndex()) && (t.index() == (2 + lastToken.index())) && 
		    				(allTokens.get(t.sentIndex()).get(t.index()-1).originalText().equals(","))){
		    			matchingTypes.get(currIndex).add(allTokens.get(t.sentIndex()).get(t.index()-1));
		    			matchingTypes.get(currIndex).add(t);
		    		}
		    		else{
		    			List<CoreLabel> newTokenList = new ArrayList<CoreLabel>();
		    			newTokenList.add(t);
		    			matchingTypes.add(newTokenList);
		    		}
		    	}
		    	
		    	//convert lists of tokens into strings
		    	List<String> namedEntityList = new ArrayList<String>();
		    	for(List<CoreLabel> namedEntity : matchingTypes){
		    		StringBuilder sb = new StringBuilder();
		    		for(CoreLabel t : namedEntity){
		    			sb.append(" ");
		    			sb.append(t.originalText());
		    		}
		    		namedEntityList.add(sb.toString().trim());
		    	}
		    	List<String> typeAndNamedEntityList = new ArrayList<String>();
		    	typeAndNamedEntityList.add(ne);
		    	typeAndNamedEntityList.addAll(namedEntityList);
		    	return typeAndNamedEntityList;
	    	}
	    	else{
	    		return new ArrayList<String>();
	    	}
	    }
	}
	
	
	private CoreLabel getTokenBeginningAtByteOffset(Annotation annotatedDocument, Integer beg){
		
		List<CoreMap> sentences = annotatedDocument.get(SentencesAnnotation.class);
		for(CoreMap sentence : sentences){
			for(CoreLabel token : sentence.get(TokensAnnotation.class)){
				if(token.beginPosition() == beg ){
					return token;
				}
			}
		}
		return null;
	}
	
	/**
	 * Given the information from a CorefMention determine the byte offsets
	 * of the whole mention and return as a knowitall Interval.
	 * @param document
	 * @param sentNum
	 * @param startIndex
	 * @param endIndex
	 * @return
	 */
	public Interval getCharIntervalFromCorefMention(Annotation document, Integer sentNum, Integer startIndex, Integer endIndex){
		
		List<CoreMap> sentences = document.get(SentencesAnnotation.class);
		CoreMap sentence = sentences.get(sentNum-1);
		List<CoreLabel> tokens = sentence.get(TokensAnnotation.class);
		List<CoreLabel> spanningTokens = new ArrayList<CoreLabel>();
		for(int i = startIndex; i < endIndex; i++){
			spanningTokens.add(tokens.get(i-1));
		}
		
		return Interval.closed(spanningTokens.get(0).beginPosition(),spanningTokens.get(spanningTokens.size()-1).endPosition());
		
	}
	
	public Interval getIntervalOfKBPEntityMention(String kbpEntityString, Interval originalInterval, String docID){
		Annotation document;
		if(corefAnnotationMap.containsKey(docID)){
			document = corefAnnotationMap.get(docID);
		}
		else{
			String xmlDoc = SolrHelper.getRawDoc(docID);
			if(xmlDoc.trim().isEmpty()){
				return null;
			}
			document = new Annotation(xmlDoc);
			try{
		     System.out.println("Annotating document "+ docID);
			 corefPipeline.annotate(document);
		     System.out.println("Done Annotating document "+ docID);
			 corefAnnotationMap.put(docID, document);
			}
			catch (Exception e){
				if(corefAnnotationMap.containsKey(docID)){
					corefAnnotationMap.remove(docID);
				}
				return null;
			}
		}

		
		//get token of possible coref mention
		CoreLabel token = getTokenBeginningAtByteOffset(document, originalInterval.start());
		if(token == null){
			return null;
		}
		Integer corefID = token.get(CorefClusterIdAnnotation.class);
		if(corefID == null){
			return null;
		}
		Map<Integer, CorefChain> graph = document.get(CorefChainAnnotation.class);
		List<CorefMention> mentionsInOrder = graph.get(corefID).getMentionsInTextualOrder();

		
		for(CorefMention corefMention : mentionsInOrder){
			if (corefMention.mentionSpan.trim().toLowerCase().equals(kbpEntityString.trim().toLowerCase())){
				// this is a match and the originalInterval corefers to the kbpEntityString
				// return the proper interval of this mention of the kbpEntityString
				return getCharIntervalFromCorefMention(document,corefMention.sentNum,corefMention.startIndex,corefMention.endIndex);
			}
		}
		return null;
	}
	
	private class AnnotationRunnable implements Runnable {
		
		Annotation doc;
		StanfordCoreNLP pipeline;
		public AnnotationRunnable(Annotation document, StanfordCoreNLP pipeline){
			doc = document;
			this.pipeline = pipeline;
		}
		public void run(){
			pipeline.annotate(doc);
		}
	}
}
