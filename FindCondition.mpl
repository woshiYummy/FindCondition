

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
	[seq(`if`(type(i,`list`),ListSiFac(i),SiFac(i)),i in Test)];
end proc:


MinimumElementIndex:=proc(F::list,
	{NoPoly::list(polynom):=[],IsSubProc::truefalse:=false,InputIndex:=[],
		ReverseSearchIndex:=[]
	})
	
	local Num, NumOfNoZero, NumOfZero, Test, LocalDegree, LocalNops,
		LocalTotalDegree, LocalMinDegree, LocalMinNops, LocalMinList,
		AimIndex, Index, TestAns, i, n;
	global MinimumList, MinNops, MinDegree, MinTotalDegree;
	
	if not IsSubProc then
		MinimumList:=[];
		MinNops:=infinity;
		MinDegree:=infinity;
		MinTotalDegree:=infinity;
	fi;
	
	if nops(ReverseSearchIndex)=0 then
		Num:=nops(F);
		NumOfNoZero:=0;
		NumOfZero:=0;
		LocalTotalDegree:=0;
		LocalMinDegree:=infinity;
		LocalMinNops:=infinity;
		LocalMinList:=[];
		for i from 1 to Num do
			Test:=op(i,F);
			if type(Test,`list`) then
				TestAns:=MinimumElementIndex(Test,
				convert("NoPoly",symbol)=NoPoly,
				convert("IsSubProc",symbol)=true,
				convert("InputIndex",symbol)=[op(InputIndex),i]);
				if whattype(TestAns)=`integer` and TestAns=0 then
					NumOfZero:=NumOfZero+1;
				elif whattype(TestAns)=`exprseq` or whattype(TestAns)=`integer` then
					return(TestAns);
				fi;
			elif Testzero(Test) then
				NumOfZero:=NumOfZero+1;
				break;
			elif type(Test,`numeric`) then
				NumOfNoZero:=NumOfNoZero+1;
				next;
			elif ListTools:-Search(Test,NoPoly)>0 then
				NumOfNoZero:=NumOfNoZero+1;
				next;
			else
				LocalDegree:=degree(Test);
				LocalNops:=nops(Test);
				LocalTotalDegree:=LocalTotalDegree+LocalDegree;
				if LocalDegree>LocalMinDegree or
				   LocalNops>LocalMinNops then
					next;
				elif LocalDegree=LocalMinDegree and
					LocalNops=LocalMinNops then
					LocalMinList:=[op(LocalMinList),[op(InputIndex),i]];
				else
					LocalMinList:=[[op(InputIndex),i]];
					LocalMinDegree:=LocalDegree;
					LocalMinNops:=LocalNops;
				fi;
			fi;
		od;
		if NumOfZero>0 and not type(Test,`list`) then
			return(0);
		elif NumOfZero=Num and type(Test,`list`) then 
			return(0);
		elif NumOfNoZero=Num and nops(InputIndex)>0 then
			return(op(InputIndex));
		elif NumOfNoZero=Num and nops(InputIndex)=0 then
			return(0);
		elif LocalTotalDegree>MinTotalDegree or
			LocalMinDegree>MinDegree or
			LocalMinNops>MinNops then
		elif LocalTotalDegree=MinTotalDegree and
			LocalMinDegree=MinDegree and
			LocalMinNops=MinNops then
			MinimumList:=[op(MinimumList),op(LocalMinList)];
		else
			MinimumList:=LocalMinList;
			MinDegree:=LocalMinDegree;
			MinNops:=LocalMinNops;
			MinTotalDegree:=LocalTotalDegree;
		fi;
	else
		Num:=nops(ReverseSearchIndex);
		NumOfZero:=0;
		for Index in ReverseSearchIndex do
			n:=nops(Index);
			AimIndex:=[op([1..n-1],Index)];
			TestAns:=MinimumElementIndex(op(AimIndex,F),
			convert("NoPoly",symbol)=NoPoly,
			convert("IsSubProc",symbol)=true,
			convert("InputIndex",symbol)=AimIndex);
			if whattype(TestAns)=`integer` and 
				TestAns=0 then
				NumOfZero:=NumOfZero+1;
			elif whattype(TestAns)=`exprseq` or 
				whattype(TestAns)=`integer` then
				return(TestAns);
			fi;
		od;
		if NumOfZero=Num then 
			return(0);
		fi;
	fi;
	
	if IsSubProc then
		return(MinimumList);
	else
		return([ListTools:-Categorize((x,y)->op(x,F)=op(y,F),MinimumList)]);
	fi;
end proc:


ListConvertMul:=proc(L::list)
	if nops(L)=0 then return(0) fi;
	convert(L,`*`);
end proc:


ListToMul:=proc(L::list)
	if nops(L)>0 and type(op(1,L),`list`) then
		return(map(ListToMul,L));
	else
		return(ListConvertMul(L));
	fi;
end proc:


ListNormalForm:=proc(NF::list, GB::list(polynom), T::ShortMonomialOrder)
	local i, j, Ans, Aim;
	if nops(NF)>0 and type(op(1,NF),`list`) then
		return(map(ListNormalForm,NF,GB,T));
	fi;
	if nops(GB)=0 then
		return(NF);
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


FindCondition:=proc( Polys::list, {IsFactored::truefalse:=false, 
	InNoZero::list:=[]
} )
	local Condition, BadCondition, NoCondition, Output, BeginTime, VList,
		GBCondition, GBTord, EndCondition, RevCondition, BeginCondition,
		RevIndex, MaxLevel, F, S, H, IndexClassList, PreIndex, PreNum, 
		VNoCondition, VGB, VTord, ConditionIndex, NumOut, i, v, k,
		FinalNoCondition, FList, IList, LocalNoCondition;
	global globalVar;
	
	#Initial
	Condition:=[];
	ConditionIndex:=[];
	BadCondition:=[];
	NoCondition:=[];
	Output:=[];
	
	BeginTime:=time[real]();
	FList:=[];
	IList:=[];
	VList:=indets(Polys);
	GBCondition:=[];
	GBTord:=op(VList);
	EndCondition:=false;
	RevCondition:=false;
	BeginCondition:=true;
	LocalNoCondition:=table();
	RevIndex:=[];
	MaxLevel:=0;
	
	while not EndCondition do
		if not RevCondition then
			#Factorization
			if BeginCondition then
				if IsFactored then
					F:=Polys;
				else
					F:=ListSiFac(Polys);
				fi;
				BeginCondition:=false;
			else
				#Simplification
				S:=ListNormalForm(F, GBCondition, 'plex'(GBTord)):
				H:=ListToMul(S):
				F:=ListSiFac(H);
			fi;
		fi;	
		
		#Searching
		IndexClassList:=MinimumElementIndex(F, 
			'NoPoly'=[op(NoCondition), op(InNoZero)], 
			'ReverseSearchIndex'=RevIndex
		);
		if whattype(IndexClassList)=`integer` and
			IndexClassList=0 then
			#All Elements in F equal zero
			FinalNoCondition:=[op(NoCondition),op(InNoZero)];
			VNoCondition:=[seq(v[i]*FinalNoCondition[i]-1,
				i=1..nops(FinalNoCondition)
			)];
			VGB:=Groebner:-Basis([op(GBCondition),op(VNoCondition)],
				'VTord'
			);
			
			if nops(VGB)=1 and op(1,VGB)=1 then
				BadCondition:=[op(BadCondition),
					[Condition, FinalNoCondition, VTord, ConditionIndex]
				];
				save BadCondition, "FindCondition_BadCondition.m";
			else
				Output:=[
					op(Output),[Condition, NoCondition, ConditionIndex]
				];
				NumOut:=nops(Output);
				save Output, "FindCondition_Output.m";
				printf("Find %dth condition! Total used time %fs.\n",
					NumOut, time[real]()-BeginTime
				);
			fi;
			if nops(Condition)=0 then EndCondition:=true; break; fi;
			RevCondition:=true;
		elif whattype(IndexClassList)=`integer` or 
			whattype(IndexClassList)=`exprseq` then
			#There is a element in F that subset NoCondition
			BadCondition:=[op(BadCondition),
				[Condition, NoCondition, [IndexClassList], ConditionIndex]
			];
			save BadCondition, "FindCondition_BadCondition.m";
			#EndCondition Judge
			if nops(Condition)=0 then EndCondition:=true; break; fi;
		   	RevCondition:=true;
		else 
			#Can find a element
			PreIndex, PreNum:=ListTools:-FindMaximalElement(
				IndexClassList, (x,y)->nops(x)<nops(y), 'position'
			);
			FList:=[op(FList),F]:
			IList:=[op(IList),PreIndex];
			Condition:=[op(Condition),
				op(op([PreNum,1],IndexClassList),F)
			];
			ConditionIndex:=[op(ConditionIndex),
				op([PreNum,1],IndexClassList)
			];
			
			GBTord:=Groebner:-SuggestVariableOrder(Condition);
			GBCondition:=Groebner:-Basis(Condition, 'plex'(GBTord));
			VNoCondition:=[seq(v[i]*NoCondition[i]-1,
				i=1..nops(NoCondition)
			)];
			VGB:=Groebner:-Basis([op(GBCondition),op(VNoCondition)],
				'VTord'
			);
			
			#Incompatible
			if nops(VGB)=1 and op(1,VGB)=1 then
				BadCondition:=[op(BadCondition),
					[Condition, NoCondition, VTord, ConditionIndex]
				];
				save BadCondition, "FindCondition_BadCondition.m";
				RevCondition:=true;
			else
				RevCondition:=false;
				RevIndex:=[];
			fi;
		fi;
		
		if RevCondition then
			#Backing
			k:=nops(Condition);
			if k>MaxLevel then MaxLevel:=k; fi;
			#Define the k th level no zero condition
			if type(LocalNoCondition[k],list) then
				LocalNoCondition[k]:=[
					op(LocalNoCondition[k]),Condition[k]
				];
			else
				LocalNoCondition[k]:=[Condition[k]];
			fi;
			for i from k+1 to MaxLevel do
				LocalNoCondition[i]:=[];
			od;
			#Define the no zero condition
			NoCondition:=[seq(`if`(
				type(LocalNoCondition[i],list),
				op(LocalNoCondition[i]),
				NULL
			), i=1..MaxLevel)];
			
			Condition:=Condition[1..k-1];
			ConditionIndex:=ConditionIndex[1..k-1];
			F:=FList[k];
			RevIndex:=IList[k];
			FList:=FList[1..k-1];
			IList:=IList[1..k-1];
		fi;
	od;
	return(Output);
end proc:




