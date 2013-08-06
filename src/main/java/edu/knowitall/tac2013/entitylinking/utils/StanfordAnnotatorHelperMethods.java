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
import java.util.regex.Pattern;

import org.apache.commons.io.IOUtils;

import edu.knowitall.collection.immutable.Interval;
import edu.stanford.nlp.dcoref.CorefChain;
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
	}
	
	public void clearHashMaps(){
		corefAnnotationMap.clear();
	}

	
	public List<CorefMention> getCorefMentions(String xmlString, Integer begOffset) {
		Annotation document = new Annotation(xmlString);
		corefPipeline.annotate(document);
		
		
		
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
	    	return graph.get(corefClusterID).getMentionsInTextualOrder();
	    }
	    else{
	    	return new ArrayList<CorefMention>();
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
	private Interval getCharIntervalFromCorefMention(Annotation document, Integer sentNum, Integer startIndex, Integer endIndex){
		
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
}
