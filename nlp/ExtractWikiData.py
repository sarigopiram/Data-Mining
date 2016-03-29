#Python program to extract TREC WIKIPEDIA  data

# glob is a python package that lists the contents of a directory.
# re is the regualr expression library in python 
import glob,re,nltk
from nltk.corpus import stopwords
from nltk.stem.wordnet import WordNetLemmatizer
#import sys  

#reload(sys)  
#sys.setdefaultencoding('utf8')
#nltk.download('all')
#set the data directory
wikidir = '/projects/class/dsba-6100/wlodeksData/trecWikipedia/*'  
#test directory
#wikidir='/media/sariram/SariRam/Spring 2015/saritha/*'


def main():
	wikidata='/users/sramkuma/outputproj/wikidata.txt'
	outputfile=open(wikidata,'w')
	title=""
	text=""
	for wikifile in glob.glob(wikidir):
		#print wikifile
		currFile=open(wikifile,'r')
		for line in currFile:
			position=currFile.tell()		
			#dataline=currFile.readline()
			dataline=line
			matchTitleTag=re.match(r"<title>",dataline)
			if matchTitleTag:
				dataline=dataline.strip('<title>')
				dataline=dataline.replace("</title>","")
				extnLookUp=re.match(r".*(jpg|png|gif|bmp|jpeg)",dataline)
				if extnLookUp:
					title=""
					continue
				else:
					filmtagbool=re.search(r'\(.*film\)',dataline)
					if filmtagbool:
						filmtag=filmtagbool.start()
						dataline=dataline[:filmtag]
				disambig=re.search(r'\(disambiguation\)',dataline)
				if disambig:
					dataline=dataline[:disambig.start()]
				title=dataline.rstrip('\n')
				#quotes=re.search(r'\"',dataline)
				if title=="":
					continue;
			matchtextTag=re.match(r'<text>',dataline)
			if matchtextTag:
				dataline=dataline.strip('<text>')
				dataline=dataline.strip('</text>')
				#print dataline
				trimdata=dataline[:500]	
				#print trimdata	
				matchObj=re.search(r'(is a )[\d]{4}.*?(film)',trimdata)
				#print matchObj
				try:
					if matchObj:
					#print dataline
					#trimdata=trimdata.encode('utf8').decode('utf8')
						subtext=matchObj.group(0)
						year=subtext[5:9]
						trimdata=re.sub("stars","starred",trimdata)
					#try:
						text_list=nltk.word_tokenize(trimdata)
						stops=set(stopwords.words('english'))
						token=text_list
						#oken = [token.lower() for token in text_list]
						stemmer =nltk.stem.porter.PorterStemmer()
					#temmer.stem(token)
					
						for tok in token:	
							if tok in stops:
								stemmer.stem(tok)
								token.remove(tok.lower())
						tokenList=token
						#print token
						lmtzr = WordNetLemmatizer()
						token=[lmtzr.lemmatize(tok) for tok in token]
						token= nltk.pos_tag(token)
 						#print token
						genre=""
						#year=""
						grammar = r"""
							#NP: {<CD>(<NN>|<JJ>)+}
							#V: {<V.*>(<N.*>|<J.*>|<RB>|<,>|<VBG>)*}"""
						parsesentence=nltk.RegexpParser(grammar)
						tree=parsesentence.parse(token)
						wordList=['write','star','produce','direct']
                                                writers=""
                                                stars=""
                                                producers=""
                                                directors=""
						boxOfficelist=""
						for subtree in tree.subtrees():
							if subtree.label()=="#NP":
							#	print subtree
								length=len(subtree.leaves())
								filmtuple=subtree.leaves()[length-1]
                                                              	if "film"==filmtuple[0]:
							#		year=subtree.leaves()[0][0]
									#length=len(subtree.leaves())
									i=1
									while i<=length-2:
										gen= subtree.leaves()[i]
										genre=genre+gen[0]+";"
										i=i+1
							if subtree.label()=="#V":
								#print subtree.leaves()
								prevStem=""
								personNames=""
								stemword=stemmer.stem(subtree.leaves()[0][0])
								if "VB" in subtree.leaves()[0][1] and stemword in wordList:
									leng=len(subtree.leaves())
									namesList=""
									if leng==1 and personNames:
										namesList=personNames
									else:
										for icount in range(1,leng):
											namesList=namesList+" "+subtree.leaves()[icount][0]
									namesList=namesList.replace(',',";")	
									if stemword=="direct":
										directors=namesList		
									if stemword=="write":
										writers=namesList		
									if stemword=="star":
										stars=namesList
									if stemword=="produce":
										producers=namesList
									personNames=namesList
						
						budgetpoint=re.search(r'=+(Box office)=*',dataline)
						if budgetpoint:
							budgetpos=budgetpoint.start()
							boxOfficeText=dataline[budgetpos+13:budgetpos+2000]
							#rint "title in boxOffice"+title
							text_list=nltk.word_tokenize(boxOfficeText)
							stops=set(stopwords.words('english'))
							token = [token.lower() for token in text_list]
							stemmer =nltk.stem.snowball.EnglishStemmer()
							lmtzr = WordNetLemmatizer()
							#print token
							for tok in token:	
								if tok in stops:
									token.remove(tok)
									
							token=[stemmer.stem(tok) for tok in token]
							newString= ','.join(token)
							newString=newString.replace("u\.s\.","us")
							listData=newString.split(",.,")
							boxOffice=extract_boxOffice(title,listData)
							#boxOfficelist=""
							for(attribute,count) in boxOffice:
								if(isinstance(count,int)):
									value=str(count)
								else:
									value=count
								boxOfficelist=boxOfficelist+value+"\t"
							print title+"=="+year+wikifile
							#print genre
							#print boxOffice
						outputfile.write(title+"\t"+year+"\t"+genre+"\t"+stars+"\t"+directors+"\t"+boxOfficelist+"\n")
						#outputfile.write(title+","+year+","+genre+","+stars+","+directors+","+producers+","+writers+","+boxOfficelist+"\n")
						#print title+"=="+year+wikifile

				except UnicodeDecodeError:
					continue;
	outputfile.close()
#------------------------------------------------END OF MAIN METHOD---------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------------------------------------------------------



#---------------------------------------------------------------------------------------------------------------------------------------------------
#-----------------------------------------FETCH BOX OFFICE DATA-------------------------------------------------------------------------------------	
def extract_boxOffice(title,boxOfficeData):
	usOpeningweek=0
        worldwideOpeningweek=0
        intlnOpeningweek=0
        usGross=0
        intlnGross=0
        worldwideGross=0
	theatreCount=0

	for item in boxOfficeData:
		usindex=-1
		usindex=-1
		usindex=-1 
		intlnIndex=-1
		worldIndex=-1
	
		#Set the booleans and index
		matchNorth=re.search(r'north',item)
		matchAmerica=re.search(r'america*',item)
		matchUnited=re.search(r'united',item)
		matchStates=re.search(r'states',item)
		matchUS=re.search(r'us',item)
		matchi8nln=re.search(r'intern*',item)
		matchWorldwide=re.search(r'worldwi*',item)
		matchTheatres=re.search(r'theater*',item)
		matchScreen=re.search(r'screen',item)
		matchOpen=re.search(r'open',item)
		matchWeekend=re.search(r'week*',item)
		matchGross=re.search(r'gross',item)
		matchRevenue=re.search(r'revenue',item)
		matchDomestic=re.search(r'domes*',item)
		matchTotal=re.search(r'total',item)
		#test=[("matchNorth",matchNorth),("matchAmerica",matchAmerica),("matchUnited",matchUnited),("matchStates",matchStates),("matchUS",matchUS),("matchi8nln",matchi8nln),("matchWorldwide",matchWorldwide),("matchTheatres",matchTheatres),("matchScreen",matchScreen),("matchOpen",matchOpen),("matchWeekend",matchWeekend),("matchGross",matchGross),("matchRevenue",matchRevenue),("matchDomestic",matchDomestic)]                
	
#---------------------------------------------------START OF THEATER FETCING------------------------------------------	
		if (matchTheatres or matchScreen and theatreCount==0):
			if matchTheatres:
				screenIndex=matchTheatres.start()
			else:
				screenIndex=matchScreen.start()
			theatreCount=item[screenIndex-6:screenIndex-1]
#---------------------------------------------------END OF THEATER FETCING------------------------------------------
#---------------------------------------------------START OF GROSS----------------------------------------------------
		#the text as gross or revenue and no open
		if ((matchGross or matchRevenue) and not(matchOpen)):   
			#The text as North America or Us or United States
			#print item
			if(matchNorth and matchAmerica):
				usindex=matchNorth.start()
			elif(matchUS):
				usindex=matchUS.start()
			elif (matchUnited and matchStates):
				usindex=matchUnited.start()  
			if matchi8nln:
				intlnIndex=matchi8nln.start()
			if matchWorldwide:
				worldIndex=matchWorldwide.start()
			elif matchTotal:
				worldIndex=matchTotal.start()
			if matchDomestic:
				usindex=matchDomestic.start()
			if((matchNorth and matchAmerica) or (matchUS) or (matchUnited and matchStates) or (matchDomestic)):
			#The text has international or worldwide
				if(matchi8nln or matchWorldwide):
					dollarIndex=re.search(r'\$',item)
					# if America appears before International and world wide, first dollar is US gross
					if (usindex>0 and usindex<intlnIndex or usindex<worldIndex):
						if dollarIndex:
							dolInd=dollarIndex.start()
							if usGross==0:
								usGross=item[dolInd+2:dolInd+14]
								if (intlnIndex>=0 and (intlnIndex<worldIndex or worldIndex<0)):
									dollarIndex=re.search(r'\$',item[dolInd+10:])
									if dollarIndex:	
										dolInd=dolInd+10+dollarIndex.start()
										if intlnGross==0:	
											intlnGross=item[dolInd+2:dolInd+14]
								if (worldIndex>=0):
									dollarIndex=re.search(r'\$',item[dolInd+10:])
									if dollarIndex:	
										dolInd=dolInd+10+dollarIndex.end()
										if worldwideGross==0:	
											worldwideGross=item[dolInd+2:dolInd+14]
					elif(usindex>0):
					#if America appears after International and world wide, first dollar is US gross
						dolInd=0
						if (intlnIndex>=0 and (intlnIndex<usindex or (intlnIndex<worldIndex or worldIndex<0))):
							dollarIndex=re.search(r'\$',item)
							if dollarIndex:	
								dolInd=dollarIndex.start()
								if intlnGross==0:	
									intlnGross=item[dolInd+2:dolInd+14]
						if (worldIndex>=0 and worldIndex<usindex):
							dollarIndex=re.search(r'\$',item[dolInd+10:])
							if dollarIndex:	
								dolInd=dolInd+10+dollarIndex.start()
								if worldwideGross==0:	
									worldwideGross=item[dolInd+2:dolInd+14]
						dollarIndex=re.search(r'\$',item)
						if dollarIndex:
							dolInd=dolInd+10+dollarIndex.start()
							if usGross==0:
								usGross=item[dolInd+2:dolInd+14]
				else:
					 dollarIndex=re.search(r'\$',item)
					 if dollarIndex:
						 dolInd=dollarIndex.start()
						 if usGross==0:
							usGross=item[dolInd+2:dolInd+14]
			elif(matchi8nln or  matchWorldwide):
				if (intlnIndex>=0 and (intlnIndex<worldIndex or worldIndex<0)):
					dollarIndex=re.search(r'\$',item)
					if dollarIndex:	
						dolInd=dollarIndex.start()
						if intlnGross==0:	
							intlnGross=item[dolInd+2:dolInd+14]
				if (worldIndex>=0):
					dollarIndex=re.search(r'\$',item)
					if dollarIndex:	
						dolInd=dollarIndex.start()
						if worldwideGross==0:	
							worldwideGross=item[dolInd+2:dolInd+14]
			#else:
			#	dollarIndex=re.search(r'\$',item)
			#	if dollarIndex:
			#	      dolInd=dollarIndex.start()
			#	      if usGross==0:
			#			usGross=item[dolInd+2:dolInd+14]
						
#---------------------------------------------------END OF GROSS--------------------------------------------------------------------------------
#---------------------------------------------------START OF OPENING WEEKEND--------------------------------------------------------------------				
		if (matchOpen and matchWeekend):
			#print item
			if(matchNorth and matchAmerica):
				usindex=matchNorth.start()
			elif(matchUS):
				usindex=matchUS.start()
			elif (matchUnited and matchStates):
				usindex=matchUnited.start()  
			if matchi8nln:
				intlnIndex=matchi8nln.start()
			elif matchWorldwide:
				intlnIndex=matchWorldwide.start()
			if matchDomestic:
                                usindex=matchDomestic.start()
                        if((matchNorth and matchAmerica) or (matchUS) or (matchUnited and matchStates) or (matchDomestic)):
                        	if(matchi8nln or matchWorldwide):
					dollarIndex=re.search(r'\$',item)
					if usindex<intlnIndex:
						if dollarIndex:
							dolInd=dollarIndex.start()
							if usOpeningweek==0:
								usOpeningweek=item[dolInd+2:dolInd+14]
								dollarIndex=re.search(r'\$',item[dolInd+10:])
						#		dolInd=dollInd+10+dollarIndex.start()
								if dollarIndex:	
									dolInd=dolInd+10+dollarIndex.start()
									if worldwideOpeningweek==0:	
										worldwideOpeningweek=item[dolInd+2:dolInd+14]
					else:
						if dollarIndex:
							dolInd=dollarIndex.start()
							if worldwideOpeningweek==0:
								worldwideOpeningweek=item[dolInd+2:dolInd+14]
								dollarIndex=re.search(r'\$',item[dolInd+10])
								if dollarIndex:
									dolInd=dolInd+10+dollarIndex.start()
									if usOpeningweek==0:
										usOpeningweek=item[dolInd+2:dolInd+14]
				else:
					 dollarIndex=re.search(r'\$',item)
                                         if dollarIndex:
						 dolInd=dollarIndex.start()
						 if usOpeningweek==0:
                                                 	usOpeningweek=item[dolInd+2:dolInd+14]
					 
				if(matchi8nln or  matchWorldwide):
					dollarIndex=re.search(r'\$',item)
                                        if dollarIndex:
						 dolInd=dollarIndex.start()
						 if worldwideOpeningweek==0:
                                                 	worldwideOpeningweek=item[dolInd+2:dolInd+14]
					
 			else:
				dollarIndex=re.search(r'\$',item)
                                if dollarIndex:
                                      dolInd=dollarIndex.start()
				      if usOpeningweek==0:
                                      		usOpeningweek=item[dolInd+2:dolInd+14]
#---------------------------------------------------END OF OPENING WEEKEND------------------------------------------------------------------------			
	outputList=[usOpeningweek,worldwideOpeningweek,theatreCount,worldwideGross,intlnGross,usGross]
	outputIndex=["usOpeningweek","worldwideOpeningweek","theatreCount","worldwideGross","intlnGross","usGross"]
	output=[]
	i=0
	#print outputList
	for figure in outputList:
		#print figure
		if not(isinstance(figure,int)):
			figure=figure.encode('utf8')	
			figure=figure.replace(',',"")
			millionMatch=re.search(r'million',figure)
			if not(millionMatch):
				millMatch=re.search(r'mill.*',figure)
				if millMatch:
					matchIndex=millMatch.start()
					figure=figure[0:matchIndex]
					figure=figure+ " million"
				else:
					#rint figure	
					figure=re.sub('[a-z]',"",figure)
		tuple=(outputIndex[i],figure)	
		i=i+1
		output.append(tuple)
		

	return output
#---------------------------------------------------END OF FUNCTION CALL BOX OFFICE FETCH----------------------------------------------------------
#--------------------------------------------------------------------------------------------------------------------------------------------------


#--------------------------------------------------------------------------------------------------------------------------------------------------
#---------------------------------------------------CALLING MAIN METHOD----------------------------------------------------------------------------
if __name__ == "__main__":
    main()
#----------------------------------------------------------END OF MAIN METHOD CALL----------------------------------------------------------------
#-------------------------------------------------------------------------------------------------------------------------------------------------							
							


													
