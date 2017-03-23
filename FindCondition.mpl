
module PgfDraw()
	option object;

	local color:="";
	local IsFill:=false;
	
	local StartXY:="";
	local EndXY:="";
	local ControlXY:="";
	
	local IsCircle:=false;
	local CircleRadius:="";
	
	local IsNode:=false;
	local NodeContent:="";

	local LineWidth:="";

	local Output:="";

	export ModuleApply::static := proc()
		Object(PgfDraw, _passed );
	end;

	export ModuleCopy::static := proc(	self::PgfDraw, proto::PgfDraw, $)
		self:-color, self:-IsFill, self:-StartXY, self:-EndXY, self:-ControlXY,
		self:-IsCircle, self:-CircleRadius, self:-IsNode, self:-NodeContent,
		self:-Output :=
		proto:-color, proto:-IsFill, proto:-StartXY, proto:-EndXY, proto:-ControlXY,
		proto:-IsCircle, proto:-CircleRadius, proto:-IsNode, proto:-NodeContent,
		proto:-Output;
	end;

	export ModulePrint::static := proc( self::PgfDraw )
		setOutput(self);
		nprintf(self:-Output);
	end;

	export setOutput::static := proc( self::PgfDraw )
		local Option;
		if self:-color <> "" then
			Option:=`if`(self:-IsFill,cat("fill=",self:-color),cat("color=",self:-color));
		else
			Option:="";
		fi;
		if self:-LineWidth <> "" then
			Option:=cat(Option, ", line width=", self:-LineWidth);
		fi;
		if not Option="" then
			Option:=cat("[", Option, "]");
		fi;
		if self:-IsCircle then
			self:-Output:=cat("\\draw ",Option,self:-StartXY," circle (",self:-CircleRadius,");");
		elif self:-IsNode then
			self:-Output:=cat("\\draw ",Option,self:-StartXY," node {",self:-NodeContent,"};");
		elif self:-ControlXY <> "" then
			self:-Output:=cat("\\draw ",Option,self:-StartXY," .. controls ",self:-ControlXY," .. ",self:-EndXY,";");
		elif self:-StartXY <>"" then
			self:-Output:=cat("\\draw ",Option,self:-StartXY," -- ",self:-EndXY,";");
		else
			self:-Output:="\\draw ";
		fi;
	end;

	export setcolor::static := proc(self::PgfDraw, s::string, $)
		self:-color := s;
	end;

	export setIsFill::static := proc(self::PgfDraw, s::truefalse, $)
		self:-IsFill := s;
	end;

	export setStartXY::static := proc(self::PgfDraw, s::string, $)
		self:-StartXY := s;
	end;

	export setEndXY::static := proc(self::PgfDraw, s::string, $)
		self:-EndXY := s;
	end;

	export setControlXY::static := proc(self::PgfDraw, s::string, $)
		self:-ControlXY := s;
	end;

	export setIsCircle::static := proc(self::PgfDraw, s::truefalse, $)
		self:-IsCircle := s;
	end;

	export setCircleRadius::static := proc(self::PgfDraw, s::string, $)
		setIsCircle(self, true);
		self:-CircleRadius := s;
	end;

	export setIsNode::static := proc(self::PgfDraw, s::truefalse, $)
		self:-IsNode := s;
	end;

	export setNodeContent::static := proc(self::PgfDraw, s::string, $)
		setIsNode(self, true);
		self:-NodeContent := s;
	end;

	export setLineWidth::static := proc(self::PgfDraw, s::string, $)
		self:-LineWidth := s;
	end;

	export getcolor::static := proc( self::{PgfDraw, list} )
		if type(self, PgfDraw) then
			return(self:-color);
		else
			map(getcolor, self);
		fi;
	end;

	export getIsFill::static := proc( self::{PgfDraw, list} )
		if type(self, PgfDraw) then
			return(self:-IsFill);
		else
			map(getIsFill, self);
		fi;
	end;

	export getStartXY::static := proc( self::{PgfDraw, list} )
		if type(self, PgfDraw) then
			return(self:-StartXY);
		else
			map(getStartXY, self);
		fi;
	end;

	export getEndXY::static := proc( self::{PgfDraw, list} )
		if type(self, PgfDraw) then
			return(self:-EndXY);
		else
			map(getEndXY, self);
		fi;
	end;

	export getControlXY::static := proc( self::{PgfDraw, list} )
		if type(self, PgfDraw) then
			return(self:-ControlXY);
		else
			map(getControlXY, self);
		fi;
	end;

	export getIsCircle::static := proc( self::{PgfDraw, list} )
		if type(self, PgfDraw) then
			return(self:-IsCircle);
		else
			map(getIsCircle, self);
		fi;
	end;

	export getCircleRadius::static := proc( self::{PgfDraw, list} )
		if type(self, PgfDraw) then
			return(self:-CircleRadius);
		else
			map(getCircleRadius, self);
		fi;
	end;
	
	export getIsNode::static := proc( self::{PgfDraw, list} )
		if type(self, PgfDraw) then
			return(self:-IsNode);
		else
			map(getIsNode, self);
		fi;
	end;

	export getNodeContent::static := proc( self::{PgfDraw, list} )
		if type(self, PgfDraw) then
			return(self:-NodeContent);
		else
			map(getNodeContent, self);
		fi;
	end;

	export getLineWidth::static := proc( self::{PgfDraw, list} )
		if type(self, PgfDraw) then
			return(self:-LineWidth);
		else
			map(getLineWidth, self);
		fi;
	end;

	export getOutput::static := proc( self::{PgfDraw, list} )
		if type(self, PgfDraw) then
			setOutput(self);
			return(self:-Output);
		else
			map(getOutput, self);
		fi;
	end;
end module:

HexToName:=proc(s::string)
	uses StringTools;
	local i;
	LowerCase(cat(seq(`if`((Ord(i) < 58 and Ord(i) > 47), Char(Ord(i)+65), i ),i in s)));
end proc:

RGBToName:=proc(s::list)
	uses ColorTools;
	try 
		RGB24ToName(RGBToRGB24(s), new=false);
	catch "unknown RGB color" :
		HexToName(RGB24ToHex(RGBToRGB24(s), 'omitoctothorpe'));
	end try;
end proc:

module TeXCommand()
	option object;

	local Name:="";
	local OptionalParameters:="";
	local RequiredParameters:=[];
	local CoordinateParameters:=[];

	export ModuleApply::static := proc()
		Object(TeXCommand, _passed );
	end;

	export ModuleCopy::static := proc(	self::TeXCommand, proto::TeXCommand, $)
		self:-Name, self:-OptionalParameters, self:-RequiredParameters, self:-CoordinateParameters :=
		proto:-Name, proto:-OptionalParameters, proto:-RequiredParameters, proto:-CoordinateParameters;
	end;

	export ModulePrint::static := proc( self::TeXCommand )
		nprintf(printTeXCommand(self));
	end;

	export setName::static := proc ( self::TeXCommand, s, $ )
		self:-Name:=s;
	end;

	export setOptionalParameters::static := proc ( self::TeXCommand, s::string, $ )
		self:-OptionalParameters:=s;
	end;

	export setRequiredParameters::static := proc ( self::TeXCommand, s::list, $ )
		self:-RequiredParameters:=s;
	end;

	export setCoordinateParameters::static := proc ( self::TeXCommand, s::list(string), $ )
		self:-CoordinateParameters:=s;
	end;

	export getName::static := proc ( self::{TeXCommand, list} )
		if type(self, list) then
			return(map(getName, self));
		else
			return(self:-Name);
		fi;
	end;

	export getOptionalParameters::static := proc ( self::{TeXCommand, list} )
		if type(self, list) then
			return(map(getOptionalParameters, self));
		else
			return(self:-OptionalParameters);
		fi;
	end;

	export getRequiredParameters::static := proc ( self::{TeXCommand, list} )
		if type(self, list) then
			return(map(getRequiredParameters, self));
		else
			return(self:-RequiredParameters);
		fi;
	end;

	export getCoordinateParameters::static := proc ( self::{TeXCommand, list} )
		if type(self, list) then
			return(map(getCoordinateParameters, self));
		else
			return(self:-CoordinateParameters);
		fi;
	end;

	export printTeXCommand::static := proc ( self::{TeXCommand,string,list} )
		local OP,RP,CP;
		if type(self, string) then
			return(self);
		fi;

		if type(self, list) then
			return(cat(op(map(printTeXCommand, self))));
		fi;
		
		if self:-Name="" then
			return("");
		fi;

		if self:-OptionalParameters="" then
			OP:="";
		else
			OP:=cat("[",self:-OptionalParameters,"]");
		fi;

		if type(self:-RequiredParameters, list) then
			if nops(self:-RequiredParameters)=0 then
				RP:="";
			else
				RP:=cat("{",op(map(printTeXCommand,self:-RequiredParameters)),"}");
			fi;
		elif type(self:-RequiredParameters, string) then
			RP:=self:-RequiredParameters;
		else
			RP:=cat(printTeXCommand(self:-RequiredParameters));
		fi;

		if nops(self:-CoordinateParameters)=0 then
			CP:="";
		else
			CP:=cat(op(self:-CoordinateParameters));
		fi;

		cat("\\",self:-Name,CP,OP,RP);
	end;
end module:

StringToTeXCommand:=proc(s::string, {IsSubProc::truefalse:=false})
	uses StringTools;
	local Out, IsTakeName, T, BraketStack, TempWord, TempChar;
	global ChaNum, LenOfStr;

	if IsSubProc then
		IsTakeName:=true;
		T:=TeXCommand();
		BraketStack:="";
	else
		ChaNum:=1;
		LenOfStr:=length(s);
		IsTakeName:=false;
	fi;
	
	TempWord:="";

	Out:=[];

	while ChaNum<= LenOfStr do
		TempChar:=s[ChaNum];

		if TempChar = "\\" then
			if IsSubProc and BraketStack="" then
				return(T);
			fi;
			Out:=[op(Out), `if`(TempWord="",NULL,TempWord)];
			ChaNum:=ChaNum+1;		
			TempWord:=StringToTeXCommand(s, ':-IsSubProc'=true);
			Out:=[op(Out), TempWord];
			TempWord:="";
			next;
		fi;

		if IsSubProc then
			if ormap(`=`, ["{", "[", "("], TempChar) then
				BraketStack:=cat(BraketStack, TempChar);
				
				if IsTakeName then
					IsTakeName:=false;
					setName(T, TempWord);
				else
					Out:=[op(Out), `if`(TempWord="",NULL,TempWord)];
				fi;
				
				TempWord:="";
				if TempChar = "{" then
					Out:=[];
				elif TempChar = "(" then
					TempWord:=cat(TempWord, TempChar);
				fi;
				ChaNum:=ChaNum+1;
				next;
				
			elif ormap(`=`, ["}", "]", ")"], TempChar) then
				if BraketStack="" then
					return(T);
				elif (BraketStack[-1]="{" and TempChar="}") then
					BraketStack:=Take(BraketStack, length(BraketStack) - 1);
					Out:=[op(Out), `if`(TempWord="",NULL,TempWord)];
					setRequiredParameters(T, [op(getRequiredParameters(T)),Out]);
					
				elif (BraketStack[-1]="[" and TempChar="]") then
					BraketStack:=Take(BraketStack, length(BraketStack) - 1);
					setOptionalParameters(T, TempWord);
	
				elif (BraketStack[-1]="(" and TempChar=")") then
					BraketStack:=Take(BraketStack, length(BraketStack) - 1);
					TempWord:=cat(TempWord, TempChar);
					setCoordinateParameters(T, [op(getCoordinateParameters(T)),TempWord]);
				else
					return(T);
				fi;
				ChaNum:=ChaNum+1;
				next;
				
			else
				if IsTakeName and IsSpace(TempChar) then
					IsTakeName:=false;
					setName(T, TempWord);
					TempWord:="";
				fi;
			fi;
		fi;
		
		TempWord:=cat(TempWord, TempChar);	
		ChaNum:=ChaNum+1;
	od;

	if type(TempWord, string) and TempWord <> "" then
		Out:=[op(Out), `if`(TempWord="",NULL,TempWord)];
	fi;
	
	if IsSubProc then
		if IsTakeName then
			setName(T, TempWord);
		fi;
		return(T);
	else
		return(Out);
	fi;
end proc:

NameColor:=proc(T::TeXCommand)
	global ColorList, ColorNames;
	local localcolor, localname, num;
	
	if not type(ColorList, list) then
		ColorList:=[];
		ColorNames:=[];
	fi;

	if getName(T) <> "color" then
		return("");
	fi;

	localcolor:=TeXCommand:-printTeXCommand(getRequiredParameters(T));
	num:=ListTools:-Search(localcolor, ColorList);
	if num > 0 then
		return(op(num, ColorNames));
	else
		ColorList:=[op(ColorList), localcolor];
		localname:=RGBToName([parse(localcolor)]);
		ColorNames:=[op(ColorNames), localname];
		return(localname);
	fi;
end proc:

GenerateColDef:=proc(fd)
	uses ColorTools;
	global ColorList, ColorNames;
	local Names, i, WC;
	Names:=GetColorNames('new' = false);
	if type(ColorList, list) and nops(ColorList)>0 then
		for i from 1 to nops(ColorNames) do
			if op(i, ColorNames) in Names then
				next;
			else
				WC:=cat("\\definecolor{",op(i, ColorNames),"}{rgb}{",op(i, ColorList),"}");
				FileTools[Text][WriteLine](fd, WC);
			fi;
		od;
	fi;
end proc:

TeXCommandToPgfDraw:=proc(T::list)
	uses StringTools;
	local L, i, j, k, iName, iRP, iOP, iCP, kName, kRP;
	L:=PgfDraw();
	for i in T do
		if type(i, TeXCommand) then
			
			iName:=getName(i);
			iRP:=getRequiredParameters(i);
			iOP:=getOptionalParameters(i);
			iCP:=getCoordinateParameters(i);
			if iName = "color" then
				setcolor(L, NameColor(i));
			elif iName = "qbezier" then
				setLineWidth(L,"1pt");
				setStartXY(L, iCP[1]);
				setControlXY(L, iCP[2]);
				setEndXY(L, iCP[3]);
			elif iName = "put" then
				setStartXY(L, iCP[1]);
				for j in iRP do
					for k in j do
						if type(k, TeXCommand) then
							kName:=getName(k);
							kRP:=TeXCommand:-printTeXCommand(getRequiredParameters(k));
							if kName = "color" then
								setcolor(L, NameColor(k));
							elif kName = "circle" then
								setCircleRadius(L, cat(convert(evalf(parse(kRP)/2,2),string), "pt"));
							elif kName = "circle*" then
								setCircleRadius(L, cat(convert(evalf(parse(kRP)/2,2),string), "pt"));
								setIsFill(L, true);
							elif kName = "makebox" then
								setNodeContent(L, kRP);
							fi;
						fi;
					od;
				od;
			fi;
		fi;
	od;
	L;
end proc:

PictureToPgf:=proc(fd, wd)
	uses StringTools, FileTools;
	local currentline, IsInPicture, EnvironmentName, BeginEnvironment, EndEnvironment, Parameters, SC, LC, TC, i;
	currentline:=readline(fd);
	IsInPicture:=false;
	EnvironmentName:="picture";
	BeginEnvironment:=cat("\\begin{",EnvironmentName,"}");
	EndEnvironment:=cat("\\end{",EnvironmentName,"}");
	TC:=NULL;
	while currentline <> 0 do
		if (Search(BeginEnvironment, currentline) > 0) then
			IsInPicture:=true;
			Parameters:=SubString(currentline, (length(BeginEnvironment) + 1)..-1);
		elif (Search(EndEnvironment, currentline) > 0) then
			IsInPicture:=false;
		fi;
		if IsInPicture then
			SC:=StringToTeXCommand(currentline);
			LC:=TeXCommandToPgfDraw(SC);
			if not getOutput(LC) = "\\draw " then 
				TC:=TC, LC; 
			fi;
		fi;
		currentline:=readline(fd);
	od;
	FileTools[Text][WriteLine](wd, "\\begin{tikzpicture}[x=1pt, y=1pt]");
	GenerateColDef(wd);
	for i in [TC] do
		FileTools[Text][WriteLine](wd, getOutput(i));
	od;
	FileTools[Text][WriteLine](wd, "\\end{tikzpicture}");
end proc:

SiFac:=proc(poly::polynom)
	local FactorAndIndex,Factors1,Factors2,Factors3,i;
	FactorAndIndex:=sqrfree(poly):
	if nops(op(2,FactorAndIndex))=0 then
		return([op(1,FactorAndIndex)]);
	fi;
	Factors1:=map2(op,1,op(2,FactorAndIndex)):
	Factors2:=map(factor,Factors1):
	Factors3:=[seq(`if`(type(i,`*`),op(i),i),i in Factors2)];
end proc:

ListSiFac:=proc(Test::list)
	local i;
	[seq(`if`(type(i,'list'),ListSiFac(i),SiFac(i)),i in Test)];
end proc:

IsListAllZero:=proc(testlist::list)
	local i,TOF;
	for i in testlist do
		if type(i, 'list') then 
			TOF:=IsListAllZero(i);
		else
			TOF:=Testzero(i);
		fi;
		if TOF then
			next; 
		else 
			return(Testzero(i));
		fi;
	od;
	true;
end proc:

ListConvertMul:=proc(L::list)
	if nops(L)=0 then return(0) fi;
	convert(L,`*`);
end proc:

ListToMul:=proc(L::list)
	if nops(L)>0 and type(op(1,L),'list') then
		return(map(ListToMul,L));
	else
		return(ListConvertMul(L));
	fi;
end proc:

ListNormalForm:=proc(NF, GB::list(polynom), T::ShortMonomialOrder)
	local i, j, Ans, Aim;
	if nops(NF)>0 and type(op(1,NF),'list') then
		return(map(ListNormalForm, args));
	else
		Ans:=NULL;
		for i in NF do
			Aim:=Groebner:-NormalForm(i, GB, T);
			if Testzero(Aim) then
				return([seq(0,j=1..nops(NF))]);
			fi;
			Ans:=Ans, Aim;
		od;
		return([Ans]);
	fi;
end proc:

IsIncompatible:=proc(Zeros::list, NonZeros::list)
	uses Groebner;
	local i, v, V, VGB, VT;

	V:=[seq(v[i]*NonZeros[i]-1, i=1..nops(NonZeros))];
	VGB:=Basis([op(Zeros),op(V)],'VT');

	if nops(VGB)=1 and whattype(op(1,VGB))=`integer` then
		return(true);
	else
		return(false);
	fi;
end proc:

module Index()
	option object;

	local Sta:=-1;
	local Ind:=[];
	local Nop:=0;
	local Deg:=0;
	local TotDeg:=0;

	export ModuleApply::static := proc()
		Object(Index, _passed );
	end;

	export ModuleCopy::static := proc(	self::Index, proto::Index, b::list, $)
		if ( _npassed < 3 ) then
			self:-Sta, self:-Ind, self:-Nop, self:-Deg, self:-TotDeg :=
			proto:-Sta, proto:-Ind, proto:-Nop, proto:-Deg, proto:-TotDeg;
		else
			self:-Sta, self:-Ind, self:-Nop, self:-Deg, self:-TotDeg := op(b);
		fi;
	end;

	export ModulePrint::static := proc( self::Index )
		nprintf("[State: %a, Index: %a, Nops: %a, Degree: %a, TotalDegree: %a]",
			self:-Sta, self:-Ind, self:-Nop, self:-Deg, self:-TotDeg);
	end;

	export setTotDeg::static := proc(self::Index, t::integer, $)
		self:-TotDeg := t;
	end;

	export setSta::static := proc(self::Index, s::integer, $)
		self:-Sta := s;
	end;

	export setInd::static := proc(self::Index, i::list(integer), $)
		self:-Ind := i;
	end;

	export getSta::static := proc( self::Index )
		self:-Sta;
	end;

	export getInd::static := proc( self::Index )
		self:-Ind;
	end;

	export getDeg::static := proc( self::Index )
		self:-Deg;
	end;

	export getNop::static := proc( self::Index )
		self:-Nop;
	end;

	export getTotDeg::static := proc( self::Index )
		self:-TotDeg;
	end;

	export `=`::static := proc( l, r, $ )
		if ( _npassed <> 2 or not l::Index or not r::Index ) then
			return false;
		end;
		evalb( l:-Deg = r:-Deg and l:-Nop = r:-Nop and l:-TotDeg = r:-TotDeg );
	end;

	export `<`::static := proc( l, r, $ )
		if ( _npassed <> 2 or not l::Index or not r::Index ) then
			return false;
		end;
		evalb( not ( l:-Deg > r:-Deg or l:-Nop > r:-Nop or l:-TotDeg > r:-TotDeg ) );
	end;
end module:

MinimumElementIndex:=proc(Fm::list, NZm::list(polynom), InputIndex::list:=[])
	local NZs, i, j, SearchIndex, LocalMinIndex, LocalMinList, LocalTotDeg, Test, TestOut;

	NZs:=NZm;

	if nops(InputIndex)<>0 then
		SearchIndex:=InputIndex;
	else
		SearchIndex:=[seq(i,i=1..nops(Fm))];
	fi;

	LocalMinList:=[];
	LocalMinIndex:=Index([-1,[],infinity,infinity,infinity]);
	LocalTotDeg:=0;
	for i in SearchIndex do
		Test:=op(i,Fm);
		if type(Test, 'list') then
			if IsListAllZero(Test) then
				next;
			fi;
			
			TestOut:=MinimumElementIndex( Test, NZs);
			
			if type(i, 'list') then
				seq(setInd(j, [op(i), op(getInd(j))]), j in TestOut);
			else
				seq(setInd(j, [i, op(getInd(j))]), j in TestOut);
			fi;
			
			if getSta(TestOut[1]) > 0 then
				return(TestOut);
			elif	( getSta(TestOut[1]) < 0 and TestOut[1] < LocalMinIndex ) then
				if ( TestOut[1] = LocalMinIndex ) then
					LocalMinList:=[op(LocalMinList), op(TestOut)];
				else
					LocalMinIndex:=TestOut[1];
					LocalMinList:=TestOut;
				fi;
			else
				next;
			fi;
		else
			TestOut:=Index([-1,[i],nops(Test),degree(Test),0]);
			if Testzero(Test) then
				setSta(TestOut, 0);
				return([TestOut]);
				
			elif type(Test, 'numeric') then
				setSta(TestOut, 1);
				
			elif ListTools:-Search(Test, NZs)>0 then
				setSta(TestOut, 2);
				
			elif ( TestOut < LocalMinIndex ) then
				LocalTotDeg := LocalTotDeg + getDeg(TestOut);
				if ( TestOut = LocalMinIndex ) then
					LocalMinList:=[op(LocalMinList), TestOut];
				else
					LocalMinIndex:=TestOut;
					LocalMinList:=[TestOut];
				fi;
			else
				LocalTotDeg:=LocalTotDeg + getDeg(TestOut);
				next;
			fi;
		fi;
	od;

	if type(TestOut, 'Index') then
		if nops(LocalMinList)=0 then
			return([TestOut]);
		else
			map(setTotDeg, LocalMinList, LocalTotDeg);
			return(LocalMinList);
		fi;
	else
		return(LocalMinList);
	fi;
end proc:

GenerateReverseIndex:=proc(Fg::list, Aim::list(Index))
	local CI, PI, PN, OutInd, PreInd, i;
	CI:=[ListTools:-Categorize((x, y) -> op(getInd(x), Fg) = op(getInd(y), Fg), Aim)];
	PI, PN:=ListTools:-FindMaximalElement(CI, (x, y) -> nops(x) < nops(y), 'position');
	PreInd:=map(getInd, PI);
	OutInd:=map2([op], [1..-2], PreInd);
	OutInd:=[seq(`if`(nops(i)=0,NULL,i), i in OutInd)];
	PreInd, OutInd;
end proc:

ColorEdges:=proc(A::list(SimpleFactor))
	local OutEdges, i, Start, End, j, Test, NumA;
	OutEdges:=NULL;
	NumA:=nops(A);
	for i in A do
		if getState(i)=0 then
			Start:=getID(i);
			End:=getFather(i);
			OutEdges:=OutEdges,{Start, End};
			j:=1;
			while End <> 0 do
				Test:=op(j,A);
				if getID(Test)=End then
					Start:=getID(Test);
					End:=getFather(Test);
					OutEdges:=OutEdges,{Start, End};
				fi;
				j:=(j mod NumA) + 1;
			od;
		else
			next;
		fi;
	od;
	{OutEdges};
end proc:

DrawTree:=proc(A::list(SimpleFactor))
	local FaS, IDS, DaS, CON, G, C;
	FaS := SimpleFactor:-getFather(A);
	IDS := SimpleFactor:-getID(A);
	DaS := SimpleFactor:-getData(A);
	CON := zip((x, y) -> x = y, IDS, DaS);
	G := GraphTheory:-Graph(convert(zip((x, y) -> {x, y}, FaS, IDS), 'set'));
	C := GraphTheory:-Graph(ColorEdges(A));
	GraphTheory:-HighlightEdges(G, C, red);
	# Graph in maple graph format,  graph in GraphTheory format, lable and data.
	GraphTheory:-DrawGraph(G, style = tree), G, CON;
end proc:

FileOutput:=proc(F, bt, TS, TSf, TL, AF)
	uses StringTools;
	local texFile, mFile, texGraphFile, fd, BeginTeXFile, EndTeXFile, GraphAndData, 
		AllOut, i, BeginSuperTabular, EndSuperTabular, Fs;
	texFile:=cat(currentdir(), kernelopts(dirsep), TS, "-FindCondition.tex");
	mFile:=cat(currentdir(), kernelopts(dirsep), TS, "-FindCondition-AllOut.m");
	texGraphFile:=cat(currentdir(), kernelopts(dirsep), TS, "-FindCondition-Graph.tex");
	
	fd := FileTools[Text][Open](texFile, create, overwrite);
	BeginTeXFile:=
		cat(
		"\\documentclass[10pt]{article}\n",
		"\\usepackage{amsmath}\n",
		"\\usepackage{pgf,tikz}\n",
		"\\usepackage[urlcolor=blue]{hyperref}\n",
		"\\usepackage{supertabular}\n",
		"\\usepackage{booktabs}\n",
		"\\begin{document}\n"
		);
	EndTeXFile:=
		cat(
		"\\end{document}\n"
		);
	BeginSuperTabular:=
		cat(
		"\\tablecaption{The labels and data. \\label{tab:FindConditionLabelsData}}\n",
		"\\tablefirsthead{\\toprule\n",
		"\\multicolumn{1}{c}{Label} & ",
		"\\multicolumn{1}{c}{Data} \\\\ \\midrule",
		"}\n",
		"\\tablehead{\n",
		"\\multicolumn{2}{r}{\\small Continued from Table \\ref{tab:FindConditionLabelsData}.}\\\\ \\midrule\n",
		"\\multicolumn{1}{c}{Label} & ",
		"\\multicolumn{1}{c}{Data} \\\\ \\midrule",
		"}\n",
		"\\tabletail{\\midrule\n",
		"\\multicolumn{2}{r}{\\small B.D.}\\\\",
		"}\n",
		"\\tablelasttail{\\bottomrule",
		"}\n",
		"\\newlength{\\leftlist}\n",
		"\\setlength{\\leftlist}{\\textwidth}\n",
		"\\addtolength{\\leftlist}{-2em}\n",
		"\\begin{supertabular}{cp{\\leftlist}}\n"
		);
	EndSuperTabular:=
		cat(
		"\\end{supertabular}\n"
		);
	
	# Begin the tex.
	FileTools[Text][WriteString](fd, BeginTeXFile);
	FileTools[Text][WriteLine](fd, "\\section*{FindCondition result}");
	FileTools[Text][WriteLine](fd, cat("The \\verb|FindCondition| begin at ",TSf,".\n"));

	Fs:=ListTools:-Flatten(F);
	Fs:=zip(`=`,Fs,[],0);
	if nops(Fs)>10 then
		Fs:=[seq(latex(Fs[i], 'output'=string),i=1..10),"\\cdots\\cdots"];
	else
		Fs:=[seq(latex(i, 'output'=string),i in Fs)];
	fi;
	FileTools[Text][WriteLine](fd,"\\begin{equation}\\label{eq:sys}");
	FileTools[Text][WriteLine](fd,"  \\begin{aligned}");
	for i from 1 to nops(Fs)-1 do
		FileTools[Text][WriteLine](fd,cat("  & ",Fs[i],", \\\\"));
	od;
	FileTools[Text][WriteLine](fd,cat("  & ",Fs[-1],"."));
	FileTools[Text][WriteLine](fd,"  \\end{aligned}");
	FileTools[Text][WriteLine](fd,"\\end{equation}\n");
	
	# If no output.
	if nops(TL)=0 then
		FileTools[Text][WriteLine](fd, "Find no condition for the systems \\eqref{eq:sys}.");
		FileTools[Text][WriteString](fd, EndTeXFile);
		FileTools[Text][Close](fd);
		return();
	else
		FileTools[Text][WriteLine](fd, 
		cat("The \\verb|FindCondition| find ", nops(TL), " conditions for the systems \\eqref{eq:sys}.\n"));
	fi;

	# If proc gets some resulte.
	FileTools[Text][WriteLine](fd, "\\begin{itemize}");
	for i from 1 to nops(TL) do
		FileTools[Text][WriteLine](fd, 
		cat("  \\item Finding the ",convert(i, 'ordinal')," condition, used ",sprintf("%g",time[real]()-bt),"s in total.\n"));
	od;
	FileTools[Text][WriteLine](fd, "\\end{itemize}\n");

	# Draw graph.
	GraphAndData := DrawTree(AF);
	# Graph in maple graph format,  graph in GraphTheory format, label and data, all factors, output factors.
	AllOut:=[GraphAndData[1], GraphAndData[3]];
	save AllOut, mFile;
	
	FileTools[Text][WriteLine](fd, "If you want know more, read the file below in Maple: ");
	FileTools[Text][WriteLine](fd, cat("\\url{",CharacterMap("\\", "/", mFile),"}\n"));
	FileTools[Text][WriteLine](fd, "After read this file in Maple, we get a list named ``\\verb|AllOut|'':");
	FileTools[Text][WriteLine](fd, "\\begin{itemize}");
	FileTools[Text][WriteLine](fd, "  \\item The first element of the \\verb|AllOut| is a picture in Maple graph format.\n");
	FileTools[Text][WriteLine](fd, "  \\item The second element of the \\verb|AllOut| is a list of labels and data.\n");
	FileTools[Text][WriteLine](fd, "\\end{itemize}\n");

	FileTools[Text][WriteLine](fd, 
	"These two elements are also shown in the figure \\ref{fig:FindConditionGraph} and the table \\ref{tab:FindConditionLabelsData}.\n");
	FileTools[Text][WriteLine](fd, cat("\nThe \\verb|FindCondition| end at ",StringTools:-FormatTime("%c"),"."));
	FileTools[Text][WriteLine](fd, "\\clearpage\n");
	
	# Output graph in tex.
	GraphTheory:-Latex(GraphAndData[2], texGraphFile, 300, 300, true, ':-style' = 'tree');
	FileTools[Text][WriteLine](fd, "\\begin{figure}[!ht]");
	FileTools[Text][WriteLine](fd, "\\centering");
	PictureToPgf(texGraphFile, fd);
	FileTools[Text][WriteLine](fd, "\\caption{The graph of conditions.}\\label{fig:FindConditionGraph}");
	FileTools[Text][WriteLine](fd, "\\end{figure}\n");

	# Output table in tex.
	if nops(AF)>100 then
		FileTools[Text][WriteLine](fd, cat(
		"There are a lot of factors. ",
		"Please read the \\verb|AllOut| file, and the second element gives the relations about labels and data.\n"));
	else
		FileTools[Text][WriteString](fd, BeginSuperTabular);
		for i in GraphAndData[3] do
			FileTools[Text][WriteString](fd, cat("  ",convert(lhs(i), string), "\t&"));
			FileTools[Text][WriteString](fd, cat(" $",latex(rhs(i), 'output'=string), "$ \\\\ \n"));
		od;
		FileTools[Text][WriteString](fd, EndSuperTabular);
	fi;

	# End the tex.
	FileTools[Text][WriteString](fd, EndTeXFile);
	FileTools[Text][Close](fd);
	FileTools[Remove](texGraphFile);
end proc:

module SimpleFactor()
	option object;

	local Layer:=0;

	local Father:=0;

	local ID:=0;

	local Data:=0;

	local State:=-1;

	local Index:=[];

	local NumberOfID::static:=0;

	export ModuleApply::static := proc()
		Object(SimpleFactor, _passed );
	end;

	export ModuleCopy::static := proc(
		self::SimpleFactor, proto::SimpleFactor, b::list, $)
		if ( _npassed < 3 ) then
			self:-Layer, self:-Father, self:-ID, self:-Data, self:-State, self:-Index :=
			proto:-Layer, proto:-Father, proto:-ID, proto:-Data, proto:-State, proto:-Index;
		else
			self:-Layer, self:-Father, self:-ID, self:-Data, self:-State, self:-Index :=op(b);
		fi;
	end;

	export ModulePrint::static := proc( self::SimpleFactor )
		nprintf("[Layer: %a, Father: %a, ID: %a, Data: %a, State: %a, Index: %a]",
		self:-Layer, self:-Father, self:-ID, self:-Data, self:-State, self:-Index);
	end;

	export grow::static := proc(proto::SimpleFactor, d, i::list, $)
		NumberOfID:=NumberOfID+1;
		Object(proto, [proto:-Layer+1, proto:-ID, NumberOfID, d, -1, i]);
	end;

	export setState::static := proc(self::SimpleFactor, s::integer, $)
		self:-State := s;
	end;

	export getData::static := proc( self::{SimpleFactor, list} )
		if type(self, SimpleFactor) then
			return(self:-Data);
		else
			map(getData, self);
		fi;
	end;

	export getFather::static := proc( self::{SimpleFactor, list} )
		if type(self, SimpleFactor) then
			return(self:-Father);
		else
			map(getFather, self);
		fi;
	end;

	export getID::static := proc( self::{SimpleFactor, list} )
		if type(self, SimpleFactor) then
			return(self:-ID);
		else
			map(getID, self);
		fi;
	end;

	export getState::static := proc( self::{SimpleFactor, list} )
		if type(self, SimpleFactor) then
			return(self:-State);
		else
			map(getState, self);
		fi;
	end;

	export getIndex::static := proc( self::{SimpleFactor, list} )
		if type(self, SimpleFactor) then
			return(self:-Index);
		else
			map(getIndex, self);
		fi;
	end;

	export getLayer::static := proc( self::SimpleFactor )
		self:-Layer;
	end;

	export getNumberOfID::static := proc( self::SimpleFactor )
		self:-NumberOfID;
	end;

end module:

CoreFind:=proc(Pi::list, IZ::list(SimpleFactor), NZ::list(SimpleFactor), INZ::list, RIN::list:=[])
	local P, F, S, MI, GI, GII, SI, SF, LocalIZ, LocalPZ, GBTord, GBPZ;

	P:=Pi;
	LocalIZ:=IZ;
	LocalPZ:=map(getData, LocalIZ);
	
	if nops(LocalIZ)>0 then
		SF:=IZ[-1];
	else
		SF:=SimpleFactor([0,0,0,0,-1,[]]);
	fi;

	if IsListAllZero(P) then
		setState(SF,0);
		if nops(LocalIZ)>0 then
			if IsIncompatible(LocalPZ, [op(map(getData,NZ)), op(INZ)]) then
				setState(SF, 2);
			fi;
		fi;
		return(SF);
	fi;

	F:=ListSiFac(P);

	MI:=MinimumElementIndex(F, [op(map(getData,NZ)), op(INZ)], RIN);

	if getSta(MI[1]) >= 0 then
		setState(SF, 1);
		return(SF);
	fi;

	GI, GII:=GenerateReverseIndex(F, MI);
	SI:=GI[1];

	SF:=grow(SF, op(SI,F), GII);

	LocalIZ:=[op(LocalIZ), SF];
	LocalPZ:=map(getData, LocalIZ);

	if IsIncompatible(LocalPZ, map(getData,NZ)) then
		setState(SF, 3);
		return(SF);
	fi;
	
	GBTord:=Groebner:-SuggestVariableOrder(LocalPZ);
	GBPZ:=Groebner:-Basis(LocalPZ, 'plex'(GBTord));
	S:=ListNormalForm(F, GBPZ, 'plex'(GBTord));

	[ListToMul(S), LocalIZ];
end proc:

FindCondition:=proc(InP::list, {InputNonZeros::list:=[]})
	local Layer, i, P, TheEnd, IsZeros, NonZeros, IsReverse, Output, ReverseIndex, CoreOut,
	LayerNonZeros, AllFactors, AllFactorsFile, begintime, TimeString, TimeStringF, TimeLog;

	Layer:=0;
	P[Layer]:=InP;
	TheEnd:=false;
	IsZeros:=[];
	NonZeros:=[];
	IsReverse:=false;
	ReverseIndex:=[];

	Output:=[];	
	AllFactors:=NULL;

	# Log File Setting
	begintime:=time[real]();
	TimeString:=StringTools:-FormatTime("%Y%m%d-%H%M%S");
	TimeStringF:=StringTools:-FormatTime("%c");
	TimeLog:=[];
	
	while not TheEnd do

		# Searching.
		if not IsReverse then
			CoreOut:=CoreFind(P[Layer], IsZeros, NonZeros, InputNonZeros, ReverseIndex);
			ReverseIndex:=[];

			# All the polynomials are zero, research the new factor.
			if type(CoreOut, 'SimpleFactor') and getState(CoreOut)=0 then
				IsReverse:=true;
				Output:=[op(Output), [IsZeros, NonZeros]];
				TimeLog:=[op(TimeLog),time[real]()];

			# Some polynomials only have nonzero factors, research the new factor.
			elif type(CoreOut, 'SimpleFactor') and getState(CoreOut)=1 then
				IsReverse:=true;

			# Inconsistency, research the new factor.
			elif type(CoreOut, 'SimpleFactor') and getState(CoreOut)=2 then
				IsReverse:=true;

			# Inconsistency in the new factor, research the new factor.
			elif type(CoreOut, 'SimpleFactor') then
				Layer:=Layer+1;
				AllFactors:=AllFactors, CoreOut;
				IsZeros:=[op(IsZeros), CoreOut];
				IsReverse:=true;

			# Go on searching.
			else
				Layer:=Layer+1;
				P[Layer]:=CoreOut[1];
				IsZeros:=CoreOut[2];
				AllFactors:=AllFactors, IsZeros[-1];
				IsReverse:=false;
			fi;

		# Researching.
		else
			# Stop judge.
			Layer:=Layer-1;
			if Layer=-1 then
				TheEnd:=true;
				break;
			fi;

			# Def NonZeros.
			LayerNonZeros:=NULL;
			for i in NonZeros do
				if getLayer(i)<=Layer+1 then
					LayerNonZeros:=LayerNonZeros, i;
				fi;
			od;
			NonZeros:=[LayerNonZeros, IsZeros[Layer+1]];
			ReverseIndex:=getIndex(IsZeros[Layer+1]);
			
			IsZeros:=IsZeros[1..Layer];
			IsReverse:=false;
		fi;
	od;

	# Save the AllFactors.
	AllFactors:=[AllFactors];

	# Output in .tex, .m and the terminal.
	FileOutput(InP, begintime, TimeString, TimeStringF, TimeLog, AllFactors);

	map(SimpleFactor:-getData, Output);
end proc:

# Example 1
# Test := [x*(x+1), x*(x^2-1), a*(x+1)];
# TestOut := FindCondition(Test);

# Example 2
# Test := [(x[1]+x[2])*(x[1]+x[3]), (x[1]+x[3])*(x[1]+x[4]), (x[1]+x[4])*(x[1]+x[5])];
# TestOut := FindCondition(Test);

# Example 3
# Test := [x[1]+x[2], x[1]+x[3], (x[1]+x[4])*(x[1]+x[5])*(x[1]+x[6])*(x[1]+x[7])];
# TestOut := FindCondition(Test);

