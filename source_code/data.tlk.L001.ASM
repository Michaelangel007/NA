;************************INCLUDE FILE*****************************
;(do not assemble stand-alone.  )
				.EB		OFF
				
;===FORMAT RULES===
@START
;
;NTALK
;*All talk records must end with the hex value $EE
;*Commands and subcommands are case sensitive
;*subcommand code is :
;*command start code is $
;*command end code is . (use after all subcommands are entered)
;*keyword code is !...so all key words would start with $!
;*Less restrictive text blocks must come after more restrictive text blocks for the same keyword. 
;
;SBASM .AZ directive
;*Any imbedded hex numbers are limited to two digits and must be padded with a leading zero (i.e. 08 not 8), and the hi-bit must be masked out as any hex number in the array will be converted to negative ascii. 				
;*To use the / character within the ASCII string, use \ as the start stop control character. For example, .AZ -\test/moretext\  .In fact, any character can be used as the delimiter 
;
;==DESIGN NOTES==
;*KEYWORD INSERT
;	*Total text block, including the size of the inserted player keyword, cannot exceed $FF bytes. To enable 16-bit, add support to .EXECUTE.KEYWORD_INSERT (NPC.SEARCH)
;	*Cannot be added to the last text block in the NPC file because the insert routine can't find the end of the text block. The second phase of the insert routine uses the $ command code as the stop value, which is expected at the start of the next text block.
;	*The text block and cannot contain the character "$" as that is used as the stop value in the second phase of the insert routine. 	
;
;*VOICE MODE
;	*Put the text block with normal mode last, so that if yell/whisper mode is active the code finds any text block with those subcommands first.
;
;*VOICE MODE & EVENT CONTINGENCY
;	*Set all text blocks for the applicable keyword to have the event contingency. Result: player will received the NPC's default response until the event flag contingency is satisified. This is good gameplay, it doesn't tip the player off that the NPC is hiding something. 
;
;========COMMANDS & SUBCOMMANDS
;$A = Adult mode required. Text only seen if player has adult mode set. 
;$I = Introduction Text, displayed automatically when talk is initiated with this NPC
;$!<WORD> = Keyword. Text will be displayed in response to the specified keyword
;$E = Contingent event. Must be followed by the hex value of the flag number and hex value of the required flag value. Seeing the text will require that event flag status. Only event flags and flag values < $80 are supported because SBASM sets the high bit on all negative ASCII string values, which is needed to comply with the Apple II's ASCII implimentation. (UPDATE: to use all event flag values, San Bergman suggests the following to work around the negative ASCII issue: "Simply split the string into multiple .AS directives, followed by the final .AZ directive.". This method should resuld in the event flag value being stored as raw hex, without the assembler modifying the high bit.)
;$H = Contingent on hour. Subcommand is followed by 2 bytes. The current hour must be >= byte #1, and must be < byte #2  
;$K = Keyword Insert. Must place code twice, once in the normal subcommand string and once embedded in the text to mark the location the player keyword should be inserted into the text block. 
;$P = Push event. Must be followed by the hex value of the flag number and hex value of the value to be pushed to the flag. Seeing the text triggers the push of the specified value to the specified event flag. Only event flags and flag values < $80 are supported because SBASM sets the high bit on all negative ASCII string values, which is needed to comply with the Apple II's ASCII implimentation. (UPDATE: to use all event flag values, San Bergman suggests the following to work around the negative ASCII issue: "Simply split the string into multiple .AS directives, followed by the final .AZ directive.". This method should resuld in the event flag value being stored as raw hex, without the assembler modifying the high bit.)
;$T = NPC termination of conversation. Next byte must be a probability represented by the 8-bit hex value which the random number must be >= in order for the termination to occur. Only probability values >= $80 are supported because SBASM sets the high bit on all negative ASCII string values, which is needed to comply with the Apple II's ASCII implimentation. (UPDATE: to use all event flag values, San Bergman suggests the following to work around the negative ASCII issue: "Simply split the string into multiple .AS directives, followed by the final .AZ directive.". This method should resuld in the event flag value being stored as raw hex, without the assembler modifying the high bit.)
;$Y = Yell mode only. Text only seen if player is in yell mode.
;$W = Whisper mode only. Text only seen if player is in whisper mode.
;$FC = End of conversation record (must be done with .BS directive, not .AZ using negative ASCII)
;
;===RETURN FLAGS===
;datagram of NTALK.SEARCH.RETURN.SUB_COMMAND.FLAGS
;
;$00 = ($E) Contingent Event Flag # (Contingent for seeing text)
;$01 = ($E) Contingent Event Flag Status Required (Contingent for seeing text)
;$02 = ($P) Push Event Flag # (seeing the text triggers an event flag to be set)
;$03 = ($P) Push Event Flag Status 
;$04 = ($Y) Yell Status ($00 = not required, $01 = required to see text)
;$05 = ($W) Whisper Status ($00 = not required, $01 = required to see text)
;$06 = ($A) Adult game mode required, or off = $00
;$07-08 ($K) Distance from command end code to the insertion point of the player keyword (2 byte value, but other parts of the code only support an $FF byte text block length)
;$09 = ($T) NPC chat termination probability represented by the 8-bit hex value which the random number must be >= in order for the termination to occur.
;$0A = ($H) the hour (24 hour format) at lower end of the permitted range.
;$0B = ($H) the minute at lower end of the permitted range. Only checked if the current hour matches the lowest hour in the range. 
;$0C = ($H) the hour (24 hour format) at upper end of the permitted range.
;$0D = ($H) the minute (BCD) at upper end of the permitted range.Only checked if the current hour matches the upper hour in the range. 
;$0E = ($N) Normal (voice mode) Status ($00 = not required, $01 = required to see text)
;
;======DISABLED COMMAND & SUBCOMMANDS====
;$R = NPC Record #
;		Not needed with the NPC Text Bubble method (ZX7) because the NPC ID is added (by binary.collator.bas) as a header byte to each NPC's talk data 
;
;$FF = End of file (must be done with .BS directive, not .AZ using negative ASCII)
;		Not needed with the NPC Text Bubble method (ZX7) because the end of file stop vaule is added (by binary.collator.bas) at the end of the collate talk data file. 			
;
;======COMMAND EXAMPLE===
;.AZ -/$DRAG:$E/,#$05,#$01,/$P/,#$10,#$01/:$W./, #$08
;	Explanation: This text for NPC Record # $08 is tied to the keyword DRAG (short for DRAGON). It will only be displayed if Event Flag $05 has the value $01. After this text is displayed, the value $01 will be pushed to Event Flag $10. Only players in Whisper mode will see this text. 
;
;-Time
;.AZ -/$!HOUR:$H/,#$11,#$05,#$11,#$50,/.test/
;	Explanation: This response is permitted if the time is >= 11:05am and < 11:50am
;
;.AZ -/$!HOUR:$H/,#$00,#$05,#$01,#$59,/.test/
;	Explanation: This response is permitted if the time is >= 12:05am and < 1:59am
;
;.AZ -/$!HOUR:$H/,#$13,#$15,#$23,#$30,/.test/
;	Explanation: This response is permitted if the time is >= 1:15pm and < 11:30pm
;
;======TROUBLESHOOTING===
;*Verify that each text block has a command end code.
;				;since the command end code is "." and periods occur in most text blocks, the abscense of a command end code usually doesn't trigger the error trap. The results vary. I've seen no text displayed and the text cut prepended. Once I got a screen of garbage; maybe the error trap was hit and there was a problem with the BRK (not sure why though as NPC.TALK runs in lower main memory)
;*Verify that command codes start with "$" and the keyword command code starts with "$!"
;
;*For event contingency, event push, and NPC terminate, verify that the hex value parameters to the subcommand comply with the range rules noted in the command list. Some must be > $80 some must be >= $80. 
;
;*Double check the format for time contingencies. See examples above. 
;
;*Make sure one-time events turn the event flag off after processing (see **WARNING** in .PROCESS.EVENT.FLAGS (NPC.SEARCH))
;
;*Bad Operand (SBASM error). Check the text block for a "/" character. Since this character terminates the ASCII string it can't be used inside the text block. I'm not sure if SBASM has an escape character to ovveride this. 
;=====================================================
@END

;================BEGIN NPC RECORD DATA============

;NPC RECORD $00
			.TF     DATA.TLK.L001.00.BIN,BIN
			.OR		$0000			**Always put before .TF directive and never use again in program

REC00

;**MINER (stand-alone)
@START

;TALK: INTRODUCTION
	;.AZ -/$I.REC00. You see a scruffy looking man/
	.AZ -/$I.You see a miner hard at work./

;TALK: NAME  
	.AZ -/$!NAME:$N.My name is Arn Krautcabin/

;TALK: JOB
	.AZ -/$!JOB:$N.I'm here to mine gold./

;TALK: JEMS
	.AZ -/$!GOLDS:$N.Yep, this vein is a really good one./,#$8D,#$8D,/I'm trying to finish up quickly though./,#$8D,#$8D,/There are bandits that sometimes make camp in caverns to the south of here./

;TALK: BANDITS
	.AZ -/$!BAND:$N.They like to store their loot here after raids./,#$8D,#$8D,/But, don't get any ideas. They've hired battle mages who will blast ye through the cavern wall./

;TALK: DEFAULT (NORMAL)
	.AZ -/$!DFL:$K.I don't know anything about your $K./
	
;TALK: BYE
	.AZ -/$!BYE.Bye./

@END



;;**MINER (traveled here from town). not really hooked up, but the conversation is setup for it. 
@START

; ;TALK: INTRODUCTION
	; ;.AZ -/$I.REC00. You see a scruffy looking man/
	; .AZ -/$I.You see a miner hard at work./

; ;TALK: NAME  
	; .AZ -/$!NAME:$N.Haven't we met before? My name is Ffred Krautcabin/

; ;TALK: JOB
	; .AZ -/$!JOB:$N.I'm here to mine gold./

; ;TALK: GOLD
	; .AZ -/$!GOLD:$N.Yep, this vein is a really good one. I'm trying to finish up quickly though. There are trolls that roam these caverns./

; ;TALK: DEFAULT (NORMAL)
	; .AZ -/$!DFL:$K.I don't know anything about your $K./
	
; ;TALK: BYE
	; .AZ -/$!BYE.Bye./

@END
	
;END OF TALK RECORD
	.BS $1,$FC
	
;----------------------------END RECORD

;NPC RECORD $08
	.TF     DATA.TLK.L001.08.BIN,BIN
	
REC08
	;.AZ -/$R./, #$30

	
;;**CASTLE: Merchant
@START
;TALK: INTRODUCTION
	.AZ -/$I.Welcome to the King's Armory./,#$8D,#$8D,/Are you here to BUY equipment or do you have something to SELL to me?/

;TALK: NAME  
	.AZ -/$!NAME:$N.My name is Orsfeld./

;TALK: JOB
	.AZ -/$!JOB:$N.I am the owner of the King's Armory./

;TALK: BUY
	.AZ -/$!BUY:$N.Very good./,#$8D,#$8D,/Here is a list of my blacksmith's latest creations:/,#$8D,#$8D,/a) dagger/,#$8D,#$8D,/b) broadsword/,#$8D,#$8D,/c) scimitar/,#$8D,#$8D,/d) war axe/
	
;TALK: C
	.AZ -/$!C:$N.Sold!/,#$8D,#$8D,/Excellent choice sir!/

;TALK: DEFAULT (NORMAL)
	.AZ -/$!DFL:Are you just here to kick my forge? Buy something or get out!/
	
;TALK: BYE
	.AZ -/$!BYE.Come again!/
@END
	
;;**TOWN: guard, entrance (left)
@START
; ;TALK: INTRODUCTION
	; .AZ -/$I:$T/,#$80,/.REC08. I have conversation text setup for testing purposes. It may not make much sense. Talk to the guard to the east of me for a conversation tutorial./

; ;TALK: NAME  
	; .AZ -/$!NAME:$N.I am a tall plainsman/

; ;TALK: JOB
	; .AZ -/$!JOB:$N.To provoke you to skip ahead many pages in the book whenever I am mentioned/

; ;TALK: BOOK
	; .AZ -/$!BOOK:$N.DragonLance/

; ;TALK: DRAGON
	; .AZ -/$!DRAG:$N.It would have been much better if I wasn't in it/

; ;TALK: DUMP
	; .AZ -/$!DUMP.It would have been much better if I wasn't in itIt would have been much better if I wasn't in itIt would have been much better if I wasn't in itIt would have been much better if I wasn't in itIt would have been much better if I wasn't in/
	
; ;TALK: RAISTLIN (YELL)
	; .AZ -/$!RAIS:$E/,#$06,#$01,/:$Y.I ain't never met nobody by that name. But, you know what you could do with that whistle? You could.../
	
; ;TALK: RAISTLIN (WHISPER)
	; .AZ -/$!RAIS:$E/,#$06,#$01,/:$P/,#$03,#$01,/:$W.He's a dark souled mage/

; ;TALK: RAISTLIN (NORMAL)
	; .AZ -/$!RAIS:$E/,#$06,#$01,/$N.Who?/

	
; ;TALK: MAGE
	; .AZ -/$!MAGE:$N.Mages are fools try again/

; ;TALK: TEST ADULT (HOUR CONTINGENT)
	; .AZ -/$!ADUL:$W$A.Adult mode is on/

; ; ;TALK: TEST HOUR (HOUR CONTINGENT)
	; ; .AZ -/$!HOUR:$H/,#$11,#$05,#$11,#$50,/.The time is between 11:05am and 11:49am. I am permitted to give this response now/

; ;TALK: TEST HOUR (HOUR CONTINGENT)
	; .AZ -/$!HOUR:$H/,#$11,#$05,#$13,#$50,/.The time is between 11:05am and 1:50pm. I am permitted to give this response now/

	
; ;TALK: TEST HOUR (NOT CONTINGENT, CATCH-ALL)  
	; .AZ -/$!HOUR:$N.I am not permitted to give this response until 11:05am and not after 1:49pm/
	
; ;TALK: CURSE (SUBCOMMAND TRIGGERED)
	; .AZ -/$!%CRS:$K.The last person who said $K to me ended up on the business end 'o me axe!/

; ;TALK: END (SUBCOMMAND TRIGGERED)
	; .AZ -/$!%END.I'm done talking to you. Bugger off./

; ;TALK: DEFAULT (YELL)
	; .AZ -/$!DFL:$Y.What's all the rumpus?? Keep hollering like that and they'll hear ye down in the nine hells!./

; ;TALK: DEFAULT (WHISPER)
	; .AZ -/$!DFL:$W.Eh? Speak up harn!/

; ;TALK: DEFAULT (NORMAL)
	; .AZ -/$!DFL:$K.I don't know anything about your $K, so shut it./

; ;TALK: BYE (ADULT)
	; .AZ -/$!BYE:$A.In all likelyhood you are a horses ass, have a nice day./

; ;TALK: BYE
	; .AZ -/$!BYE.In all likelyhood you are a horses rear end, have a nice day./
@END

	
;END OF TALK RECORD
	.BS $1,$FC

;----------------------------END RECORD

;NPC RECORD $10
	.TF     DATA.TLK.L001.10.BIN,BIN
	
REC10 ;orc shaman

;TALK: INTRODUCTION
	.AZ -/$I.You see an Orc Shaman in full ceremonial garb./,#$8D,#$8D,/"Fools! What are you doing here?" he growls./,#$8D,#$8D,/"You'll never leave here alive!"/

;TALK: NAME  
	.AZ -/$!NAME:$N.My name is - none of your business./

;TALK: JOB
	.AZ -/$!JOB:$N.Robbin' and Stealin'/

;TALK: VILLAGE
	.AZ -/$!VILL:$N.Urga! Yes! We sacked that stinkin' village./
	
;TALK: SURRENDER
	.AZ -/$!SURR:$N.Never!/		
	
;TALK: DEFAULT (NORMAL)
	.AZ -/$!DFL:$K.I don't know anything about your $K./
	
;TALK: BYE
	.AZ -/$!BYE. /

	
;END OF TALK RECORD
	.BS $1,$FC
	
;----------------------------END RECORD

;;REC10 (setup for villager in combat video)
@START
; ;TALK: INTRODUCTION
	; .AZ -/$I.Those blasted orcs sacked our village! I barely made it out alive!/

; ;TALK: NAME  
	; .AZ -/$!NAME:$N.My name is Ffred Krautcabin/

; ;TALK: JOB
	; .AZ -/$!JOB:$N.Um, I work for the fire department/

; ;TALK: ORCS
	; .AZ -/$!ORCS:$N.They've been seen coming out of the caves, near that old ruined castle. Can you help us?/
	
; ;TALK: YES
	; .AZ -/$!YES:$N.Thank you! I'll see to it that the Margrave rewards you well./		
	
; ;TALK: DEFAULT (NORMAL)
	; .AZ -/$!DFL:$K.I don't know anything about your $K./
	
; ;TALK: BYE
	; .AZ -/$!BYE.Bye. Have fun raiding the castle!/

	
; ;END OF TALK RECORD
	; .BS $1,$FC
	
; ;----------------------------END RECORD
@END

;NPC RECORD $18
	.TF     DATA.TLK.L001.18.BIN,BIN
	
REC18 
;****CASTLE: ARCHER SHOW-OFF****
@START
;TALK: INTRODUCTION
	.AZ -/$I.Did you see that???/,#$8D,/Did you see that???/,#$8D,#$8D,/I just hit four bullseyes in a row!/

;TALK: NAME  
	.AZ -/$!NAME:$N.What? You don't know who I am?/,#$8D,#$8D,/You have never heard of the marvalous Lord Chiverous?/

;TALK: JOB
	.AZ -/$!JOB:$N.Jobs are for peasants. I am a Lord./,#$8D,#$8D,/Today I am practicing for the archery contest./

;TALK: CONTEST
	.AZ -/$!CONT:$N.The contest is this afternoon./,#$8D,#$8D,/The king himself will be in attendance!/

;TALK: CONTEST (YELL)
	.AZ -/$!CONT:$Y.Why are you yelling?/,#$8D,#$8D,/The whole castle can hear you!"/

;TALK: DEFAULT (NORMAL)
	.AZ -/$!DFL:$K.I have no time for your mindless prattle about $K./
	
;TALK: BYE
	.AZ -/$!BYE.Come back this afternoon!/
@END



;;****TOWN: IRENE****
@START
; ;TALK: INTRODUCTION
	; .AZ -/$I.You see a middle age woman carrying a tray with glasses on it./

; ;TALK: NAME  
	; .AZ -/$!NAME:$N.My name is Irene. Who's asking?/

; ;TALK: JOB
	; .AZ -/$!JOB:$N.I run this pub and listen to gossip./

; ;TALK: GOSSIP
	; .AZ -/$!GOSS:$N.Now, I don't repeat gossip, so listen carefully./
	
; ;TALK: OJITHAR
	; .AZ -/$!OJIT:$N.He's an advisor to the Margrave. They usually talk shortly before lunch./

; ;TALK: MARGRAVE
	; .AZ -/$!MARG:$N.Margrave Rajenor? He runs this city. His house is just north of the gates./

; ;TALK: DEFAULT (NORMAL)
	; .AZ -/$!DFL:$K.I don't know anything about your $K./
	
; ;TALK: BYE
	; .AZ -/$!BYE.What, no tip?./  ;***ADD KEYWORD 'TIP': I'll give you a tip.....
@END
	
;END OF TALK RECORD
	.BS $1,$FC

	
;----------------------------END RECORD

;NPC RECORD $20
	.TF     DATA.TLK.L001.20.BIN,BIN
	
REC20

;TALK: INTRODUCTION
	.AZ -/$I.Hi, I have the default converstion text. There are five people in town with custom conversations. Everyone else will say what I say./,#$8D,#$8D,/You can ask me things like name, work, or other keywords in my responses/,#$8D,#$8D,/Press tab to toggle voice mode./

;TALK: NAME  
	.AZ -/$!NAME:$N.My name is Ffred Krautcabin/

;TALK: JOB
	.AZ -/$!JOB:$N.To introduce NPC conversation mode in Nox Archaist/

;TALK: CONVERSATION
	.AZ -/$!CONV:$N.We're having a convesation right now. Isn't it fun? :-)/,#$8D,#$8D,/Try asking me about "Grumblebottom" in each voice mode (normal-whisper-yell)/

	
;TALK: NPC
	.AZ -/$!NPC:$N.We're having a convesation right now. Isn't it fun? :-)/,#$8D,#$8D,/Try asking me about "Grumblebottom" in each voice mode (normal-whisper-yell)/

;TALK: GRUMBLEBOTTOM (YELL)
	.AZ -/$!GRUM:$Y.Why are you yelling? The whole town can hear you!"/
	
;TALK: RAISTLIN (WHISPER)
	.AZ -/$!GRUM:$W.Ah, Grumblebottom. He is a grumpy dwarf that hangs about near the town hall around 10am./,#$8D,#$8D,/To change the time using playtest key shift+9 then enter 1000. You can set other times too using 24 hour format/
	
;TALK: RAISTLIN (NORMAL)
	.AZ -/$!GRUM.Who? I don't know what you are talking about/

;TALK: TOWN HALL
	.AZ -/$!TOW:$N.The town hall is the building directly north of the town entrance./,#$8D,#$8D,/Grumblebottom hangs out a little west of the town hall entrance./
		
	
;TALK: DEFAULT (YELL)
	.AZ -/$!DFL:$Y.Why are you yelling?/

;TALK: DEFAULT (WHISPER)
	.AZ -/$!DFL:$W.I can't hear you, what did you say?/

;TALK: DEFAULT (NORMAL)
	.AZ -/$!DFL:$K.I don't know anything about your $K./
	
;TALK: BYE
	.AZ -/$!BYE.Bye. Enjoy our test town./

	
;END OF TALK RECORD
	.BS $1,$FC
	
;----------------------------END RECORD

;NPC RECORD $28
	.TF     DATA.TLK.L001.28.BIN,BIN
	
REC28

;TALK: INTRODUCTION
	.AZ -/$I.Hi, I have the default converstion text. There are five people in town with custom conversations. Everyone else will say what I say./,#$8D,#$8D,/You can ask me things like name, work, or other keywords in my responses/,#$8D,#$8D,/Press tab to toggle voice mode./

;TALK: NAME  
	.AZ -/$!NAME:$N.My name is Ffred Krautcabin/

;TALK: JOB
	.AZ -/$!JOB:$N.To introduce NPC conversation mode in Nox Archaist/

;TALK: CONVERSATION
	.AZ -/$!CONV:$N.We're having a convesation right now. Isn't it fun? :-)/,#$8D,#$8D,/Try asking me about "Grumblebottom" in each voice mode (normal-whisper-yell)/

	
;TALK: NPC
	.AZ -/$!NPC:$N.We're having a convesation right now. Isn't it fun? :-)/,#$8D,#$8D,/Try asking me about "Grumblebottom" in each voice mode (normal-whisper-yell)/

;TALK: GRUMBLEBOTTOM (YELL)
	.AZ -/$!GRUM:$Y.Why are you yelling? The whole town can hear you!"/
	
;TALK: RAISTLIN (WHISPER)
	.AZ -/$!GRUM:$W.Ah, Grumblebottom. He is a grumpy dwarf that hangs about near the town hall around 10am./,#$8D,#$8D,/To change the time using playtest key shift+9 then enter 1000. You can set other times too using 24 hour format/
	
;TALK: RAISTLIN (NORMAL)
	.AZ -/$!GRUM.Who? I don't know what you are talking about/

;TALK: TOWN HALL
	.AZ -/$!TOW:$N.The town hall is the building directly north of the town entrance./,#$8D,#$8D,/Grumblebottom hangs out a little west of the town hall entrance./
		
	
;TALK: DEFAULT (YELL)
	.AZ -/$!DFL:$Y.Why are you yelling?/

;TALK: DEFAULT (WHISPER)
	.AZ -/$!DFL:$W.I can't hear you, what did you say?/

;TALK: DEFAULT (NORMAL)
	.AZ -/$!DFL:$K.I don't know anything about your $K./
	
;TALK: BYE
	.AZ -/$!BYE.Bye. Enjoy our test town./

	
;END OF TALK RECORD
	.BS $1,$FC
	
;----------------------------END RECORD

;NPC RECORD $30
	.TF     DATA.TLK.L001.30.BIN,BIN
	
REC30

;TALK: INTRODUCTION
	.AZ -/$I.Hi, I have the default converstion text. There are five people in town with custom conversations. Everyone else will say what I say./,#$8D,#$8D,/You can ask me things like name, work, or other keywords in my responses/,#$8D,#$8D,/Press tab to toggle voice mode./

;TALK: NAME  
	.AZ -/$!NAME:$N.My name is Ffred Krautcabin/

;TALK: JOB
	.AZ -/$!JOB:$N.To introduce NPC conversation mode in Nox Archaist/

;TALK: CONVERSATION
	.AZ -/$!CONV:$N.We're having a convesation right now. Isn't it fun? :-)/,#$8D,#$8D,/Try asking me about "Grumblebottom" in each voice mode (normal-whisper-yell)/

	
;TALK: NPC
	.AZ -/$!NPC:$N.We're having a convesation right now. Isn't it fun? :-)/,#$8D,#$8D,/Try asking me about "Grumblebottom" in each voice mode (normal-whisper-yell)/

;TALK: GRUMBLEBOTTOM (YELL)
	.AZ -/$!GRUM:$Y.Why are you yelling? The whole town can hear you!"/
	
;TALK: RAISTLIN (WHISPER)
	.AZ -/$!GRUM:$W.Ah, Grumblebottom. He is a grumpy dwarf that hangs about near the town hall around 10am./,#$8D,#$8D,/To change the time using playtest key shift+9 then enter 1000. You can set other times too using 24 hour format/
	
;TALK: RAISTLIN (NORMAL)
	.AZ -/$!GRUM.Who? I don't know what you are talking about/

;TALK: TOWN HALL
	.AZ -/$!TOW:$N.The town hall is the building directly north of the town entrance./,#$8D,#$8D,/Grumblebottom hangs out a little west of the town hall entrance./
		
	
;TALK: DEFAULT (YELL)
	.AZ -/$!DFL:$Y.Why are you yelling?/

;TALK: DEFAULT (WHISPER)
	.AZ -/$!DFL:$W.I can't hear you, what did you say?/

;TALK: DEFAULT (NORMAL)
	.AZ -/$!DFL:$K.I don't know anything about your $K./
	
;TALK: BYE
	.AZ -/$!BYE.Bye. Enjoy our test town./

	
;END OF TALK RECORD
	.BS $1,$FC
	
;----------------------------END RECORD

;NPC RECORD $38
	.TF     DATA.TLK.L001.38.BIN,BIN
	
REC38 ;GUARD #1 WHO OPENS DOOR
;TALK: INTRODUCTION
	.AZ -/$I.You see a young and rather timid looking guard./

;TALK: NAME  
	.AZ -/$!NAME.My name is Jedson./

;TALK: JOB
	.AZ -/$!JOB.To guard this door. I'm new to this job./

;TALK: OPEN (YELL)
	.AZ -/$!OPEN:$Y$P/,#$02,#$01,/.Ok, ok. Settle down/ 

;TALK: OPEN (NORMAL)
	.AZ -/$!OPEN.Well, I don't really think I should. I'll get in trouble./

;-------------

;TALK: DEFAULT (NORMAL)
	.AZ -/$!DFL.Jedson shrugs his shoulders./
	
;TALK: BYE
	.AZ -/$!BYE.See ya./

	
;END OF TALK RECORD
	.BS $1,$FC
;----------------------------END RECORD

;NPC RECORD $40
	.TF     DATA.TLK.L001.40.BIN,BIN
	
REC40 ;GUARD #1 WHO OPENS DOOR
;TALK: INTRODUCTION
	.AZ -/$I.You see a young and rather timid looking guard./

;TALK: NAME  
	.AZ -/$!NAME.My name is Jedson./

;TALK: JOB
	.AZ -/$!JOB.To guard this door. I'm new to this job./

;TALK: OPEN (YELL)
	.AZ -/$!OPEN:$Y$P/,#$02,#$01,/.Ok, ok. Settle down/ 

;TALK: OPEN (NORMAL)
	.AZ -/$!OPEN.Well, I don't really think I should. I'll get in trouble./

	
;---------------
	
;TALK: DEFAULT (NORMAL)
	.AZ -/$!DFL.Jedson shrugs his shoulders./

;TALK: BYE
	.AZ -/$!BYE. See ya./
	
;END OF TALK RECORD
	.BS $1,$FC
;----------------------------END RECORD

;NPC RECORD $48
	.TF     DATA.TLK.L001.48.BIN,BIN
	
REC48

;TALK: INTRODUCTION
	.AZ -/$I.Hi, I have the default converstion text. There are five people in town with custom conversations. Everyone else will say what I say./,#$8D,#$8D,/You can ask me things like name, work, or other keywords in my responses/,#$8D,#$8D,/Press tab to toggle voice mode./

;TALK: NAME  
	.AZ -/$!NAME:$N.My name is Ffred Krautcabin/

;TALK: JOB
	.AZ -/$!JOB:$N.To introduce NPC conversation mode in Nox Archaist/

;TALK: CONVERSATION
	.AZ -/$!CONV:$N.We're having a convesation right now. Isn't it fun? :-)/,#$8D,#$8D,/Try asking me about "Grumblebottom" in each voice mode (normal-whisper-yell)/

	
;TALK: NPC
	.AZ -/$!NPC:$N.We're having a convesation right now. Isn't it fun? :-)/,#$8D,#$8D,/Try asking me about "Grumblebottom" in each voice mode (normal-whisper-yell)/

;TALK: GRUMBLEBOTTOM (YELL)
	.AZ -/$!GRUM:$Y.Why are you yelling? The whole town can hear you!"/
	
;TALK: RAISTLIN (WHISPER)
	.AZ -/$!GRUM:$W.Ah, Grumblebottom. He is a grumpy dwarf that hangs about near the town hall around 10am./,#$8D,#$8D,/To change the time using playtest key shift+9 then enter 1000. You can set other times too using 24 hour format/
	
;TALK: RAISTLIN (NORMAL)
	.AZ -/$!GRUM.Who? I don't know what you are talking about/

;TALK: TOWN HALL
	.AZ -/$!TOW:$N.The town hall is the building directly north of the town entrance./,#$8D,#$8D,/Grumblebottom hangs out a little west of the town hall entrance./
		
	
;TALK: DEFAULT (YELL)
	.AZ -/$!DFL:$Y.Why are you yelling?/

;TALK: DEFAULT (WHISPER)
	.AZ -/$!DFL:$W.I can't hear you, what did you say?/

;TALK: DEFAULT (NORMAL)
	.AZ -/$!DFL:$K.I don't know anything about your $K./
	
;TALK: BYE
	.AZ -/$!BYE.Bye. Enjoy our test town./

	
;END OF TALK RECORD
	.BS $1,$FC
	
;----------------------------END RECORD

;NPC RECORD $50
	.TF     DATA.TLK.L001.50.BIN,BIN
	
REC50

;TALK: INTRODUCTION
	.AZ -/$I.Hi, I have the default converstion text. There are five people in town with custom conversations. Everyone else will say what I say./,#$8D,#$8D,/You can ask me things like name, work, or other keywords in my responses/,#$8D,#$8D,/Press tab to toggle voice mode./

;TALK: NAME  
	.AZ -/$!NAME:$N.My name is Ffred Krautcabin/

;TALK: JOB
	.AZ -/$!JOB:$N.To introduce NPC conversation mode in Nox Archaist/

;TALK: CONVERSATION
	.AZ -/$!CONV:$N.We're having a convesation right now. Isn't it fun? :-)/,#$8D,#$8D,/Try asking me about "Grumblebottom" in each voice mode (normal-whisper-yell)/

	
;TALK: NPC
	.AZ -/$!NPC:$N.We're having a convesation right now. Isn't it fun? :-)/,#$8D,#$8D,/Try asking me about "Grumblebottom" in each voice mode (normal-whisper-yell)/

;TALK: GRUMBLEBOTTOM (YELL)
	.AZ -/$!GRUM:$Y.Why are you yelling? The whole town can hear you!"/
	
;TALK: RAISTLIN (WHISPER)
	.AZ -/$!GRUM:$W.Ah, Grumblebottom. He is a grumpy dwarf that hangs about near the town hall around 10am./,#$8D,#$8D,/To change the time using playtest key shift+9 then enter 1000. You can set other times too using 24 hour format/
	
;TALK: RAISTLIN (NORMAL)
	.AZ -/$!GRUM.Who? I don't know what you are talking about/

;TALK: TOWN HALL
	.AZ -/$!TOW:$N.The town hall is the building directly north of the town entrance./,#$8D,#$8D,/Grumblebottom hangs out a little west of the town hall entrance./
		
	
;TALK: DEFAULT (YELL)
	.AZ -/$!DFL:$Y.Why are you yelling?/

;TALK: DEFAULT (WHISPER)
	.AZ -/$!DFL:$W.I can't hear you, what did you say?/

;TALK: DEFAULT (NORMAL)
	.AZ -/$!DFL:$K.I don't know anything about your $K./
	
;TALK: BYE
	.AZ -/$!BYE.Bye. Enjoy our test town./

	
;END OF TALK RECORD
	.BS $1,$FC
	
;----------------------------END RECORD

;NPC RECORD $58
	.TF     DATA.TLK.L001.58.BIN,BIN
	
REC58

;TALK: INTRODUCTION
	.AZ -/$I.Hi, I have the default converstion text. There are five people in town with custom conversations. Everyone else will say what I say./,#$8D,#$8D,/You can ask me things like name, work, or other keywords in my responses/,#$8D,#$8D,/Press tab to toggle voice mode./

;TALK: NAME  
	.AZ -/$!NAME:$N.My name is Ffred Krautcabin/

;TALK: JOB
	.AZ -/$!JOB:$N.To introduce NPC conversation mode in Nox Archaist/

;TALK: CONVERSATION
	.AZ -/$!CONV:$N.We're having a convesation right now. Isn't it fun? :-)/,#$8D,#$8D,/Try asking me about "Grumblebottom" in each voice mode (normal-whisper-yell)/

	
;TALK: NPC
	.AZ -/$!NPC:$N.We're having a convesation right now. Isn't it fun? :-)/,#$8D,#$8D,/Try asking me about "Grumblebottom" in each voice mode (normal-whisper-yell)/

;TALK: GRUMBLEBOTTOM (YELL)
	.AZ -/$!GRUM:$Y.Why are you yelling? The whole town can hear you!"/
	
;TALK: RAISTLIN (WHISPER)
	.AZ -/$!GRUM:$W.Ah, Grumblebottom. He is a grumpy dwarf that hangs about near the town hall around 10am./,#$8D,#$8D,/To change the time using playtest key shift+9 then enter 1000. You can set other times too using 24 hour format/
	
;TALK: RAISTLIN (NORMAL)
	.AZ -/$!GRUM.Who? I don't know what you are talking about/

;TALK: TOWN HALL
	.AZ -/$!TOW:$N.The town hall is the building directly north of the town entrance./,#$8D,#$8D,/Grumblebottom hangs out a little west of the town hall entrance./
		
	
;TALK: DEFAULT (YELL)
	.AZ -/$!DFL:$Y.Why are you yelling?/

;TALK: DEFAULT (WHISPER)
	.AZ -/$!DFL:$W.I can't hear you, what did you say?/

;TALK: DEFAULT (NORMAL)
	.AZ -/$!DFL:$K.I don't know anything about your $K./
	
;TALK: BYE
	.AZ -/$!BYE.Bye. Enjoy our test town./

	
;END OF TALK RECORD
	.BS $1,$FC
	
;----------------------------END RECORD

;NPC RECORD $60
	.TF     DATA.TLK.L001.60.BIN,BIN
	
REC60

;TALK: INTRODUCTION
	.AZ -/$I.Hi, I have the default converstion text. There are five people in town with custom conversations. Everyone else will say what I say./,#$8D,#$8D,/You can ask me things like name, work, or other keywords in my responses/,#$8D,#$8D,/Press tab to toggle voice mode./

;TALK: NAME  
	.AZ -/$!NAME:$N.My name is Ffred Krautcabin/

;TALK: JOB
	.AZ -/$!JOB:$N.To introduce NPC conversation mode in Nox Archaist/

;TALK: CONVERSATION
	.AZ -/$!CONV:$N.We're having a convesation right now. Isn't it fun? :-)/,#$8D,#$8D,/Try asking me about "Grumblebottom" in each voice mode (normal-whisper-yell)/

	
;TALK: NPC
	.AZ -/$!NPC:$N.We're having a convesation right now. Isn't it fun? :-)/,#$8D,#$8D,/Try asking me about "Grumblebottom" in each voice mode (normal-whisper-yell)/

;TALK: GRUMBLEBOTTOM (YELL)
	.AZ -/$!GRUM:$Y.Why are you yelling? The whole town can hear you!"/
	
;TALK: RAISTLIN (WHISPER)
	.AZ -/$!GRUM:$W.Ah, Grumblebottom. He is a grumpy dwarf that hangs about near the town hall around 10am./,#$8D,#$8D,/To change the time using playtest key shift+9 then enter 1000. You can set other times too using 24 hour format/
	
;TALK: RAISTLIN (NORMAL)
	.AZ -/$!GRUM.Who? I don't know what you are talking about/

;TALK: TOWN HALL
	.AZ -/$!TOW:$N.The town hall is the building directly north of the town entrance./,#$8D,#$8D,/Grumblebottom hangs out a little west of the town hall entrance./
		
	
;TALK: DEFAULT (YELL)
	.AZ -/$!DFL:$Y.Why are you yelling?/

;TALK: DEFAULT (WHISPER)
	.AZ -/$!DFL:$W.I can't hear you, what did you say?/

;TALK: DEFAULT (NORMAL)
	.AZ -/$!DFL:$K.I don't know anything about your $K./
	
;TALK: BYE
	.AZ -/$!BYE.Bye. Enjoy our test town./

	
;END OF TALK RECORD
	.BS $1,$FC
	
;----------------------------END RECORD

;NPC RECORD $68
	.TF     DATA.TLK.L001.68.BIN,BIN
	
REC68 
;TALK: INTRODUCTION
	.AZ -/$I.REC68 You see a scruffy looking man/

;TALK: NAME  
	.AZ -/$!NAME:$N.My name is Ffred Krautcabin/

;TALK: JOB
	.AZ -/$!JOB:$N.I am a miner/

;TALK: MINER
	.AZ -/$!MINE:$N.I delve deep underground and seek out veins of gems./

;TALK: UNDERGROUND
	.AZ -/$!UNDE:$N.Many places. I've heard about a rich vein of gold that is accessible from a cave to the northeast of here./

;TALK: DEFAULT (NORMAL)
	.AZ -/$!DFL:$K.I don't know anything about your $K./
	
;TALK: BYE
	.AZ -/$!BYE.Bye./

	
;END OF TALK RECORD


	.BS $1,$FC
;----------------------------END RECORD

;NPC RECORD $70
	.TF     DATA.TLK.L001.70.BIN,BIN
	
REC70

;TALK: INTRODUCTION
	.AZ -/$I.Hi, I have the default converstion text. There are five people in town with custom conversations. Everyone else will say what I say./,#$8D,#$8D,/You can ask me things like name, work, or other keywords in my responses/,#$8D,#$8D,/Press tab to toggle voice mode./

;TALK: NAME  
	.AZ -/$!NAME:$N.My name is Ffred Krautcabin/

;TALK: JOB
	.AZ -/$!JOB:$N.To introduce NPC conversation mode in Nox Archaist/

;TALK: CONVERSATION
	.AZ -/$!CONV:$N.We're having a convesation right now. Isn't it fun? :-)/,#$8D,#$8D,/Try asking me about "Grumblebottom" in each voice mode (normal-whisper-yell)/

	
;TALK: NPC
	.AZ -/$!NPC:$N.We're having a convesation right now. Isn't it fun? :-)/,#$8D,#$8D,/Try asking me about "Grumblebottom" in each voice mode (normal-whisper-yell)/

;TALK: GRUMBLEBOTTOM (YELL)
	.AZ -/$!GRUM:$Y.Why are you yelling? The whole town can hear you!"/
	
;TALK: RAISTLIN (WHISPER)
	.AZ -/$!GRUM:$W.Ah, Grumblebottom. He is a grumpy dwarf that hangs about near the town hall around 10am./,#$8D,#$8D,/To change the time using playtest key shift+9 then enter 1000. You can set other times too using 24 hour format/
	
;TALK: RAISTLIN (NORMAL)
	.AZ -/$!GRUM.Who? I don't know what you are talking about/

;TALK: TOWN HALL
	.AZ -/$!TOW:$N.The town hall is the building directly north of the town entrance./,#$8D,#$8D,/Grumblebottom hangs out a little west of the town hall entrance./
		
	
;TALK: DEFAULT (YELL)
	.AZ -/$!DFL:$Y.Why are you yelling?/

;TALK: DEFAULT (WHISPER)
	.AZ -/$!DFL:$W.I can't hear you, what did you say?/

;TALK: DEFAULT (NORMAL)
	.AZ -/$!DFL:$K.I don't know anything about your $K./
	
;TALK: BYE
	.AZ -/$!BYE.Bye. Enjoy our test town./

	
;END OF TALK RECORD
	.BS $1,$FC
	
;----------------------------END RECORD

;NPC RECORD $78
	.TF     DATA.TLK.L001.78.BIN,BIN
	
REC78

;TALK: INTRODUCTION
	.AZ -/$I.Hi, I have the default converstion text. There are five people in town with custom conversations. Everyone else will say what I say./,#$8D,#$8D,/You can ask me things like name, work, or other keywords in my responses/,#$8D,#$8D,/Press tab to toggle voice mode./

;TALK: NAME  
	.AZ -/$!NAME:$N.My name is Ffred Krautcabin/

;TALK: JOB
	.AZ -/$!JOB:$N.To introduce NPC conversation mode in Nox Archaist/

;TALK: CONVERSATION
	.AZ -/$!CONV:$N.We're having a convesation right now. Isn't it fun? :-)/,#$8D,#$8D,/Try asking me about "Grumblebottom" in each voice mode (normal-whisper-yell)/

	
;TALK: NPC
	.AZ -/$!NPC:$N.We're having a convesation right now. Isn't it fun? :-)/,#$8D,#$8D,/Try asking me about "Grumblebottom" in each voice mode (normal-whisper-yell)/

;TALK: GRUMBLEBOTTOM (YELL)
	.AZ -/$!GRUM:$Y.Why are you yelling? The whole town can hear you!"/
	
;TALK: RAISTLIN (WHISPER)
	.AZ -/$!GRUM:$W.Ah, Grumblebottom. He is a grumpy dwarf that hangs about near the town hall around 10am./,#$8D,#$8D,/To change the time using playtest key shift+9 then enter 1000. You can set other times too using 24 hour format/
	
;TALK: RAISTLIN (NORMAL)
	.AZ -/$!GRUM.Who? I don't know what you are talking about/

;TALK: TOWN HALL
	.AZ -/$!TOW:$N.The town hall is the building directly north of the town entrance./,#$8D,#$8D,/Grumblebottom hangs out a little west of the town hall entrance./
		
	
;TALK: DEFAULT (YELL)
	.AZ -/$!DFL:$Y.Why are you yelling?/

;TALK: DEFAULT (WHISPER)
	.AZ -/$!DFL:$W.I can't hear you, what did you say?/

;TALK: DEFAULT (NORMAL)
	.AZ -/$!DFL:$K.I don't know anything about your $K./
	
;TALK: BYE
	.AZ -/$!BYE.Bye. Enjoy our test town./

	
;END OF TALK RECORD
	.BS $1,$FC
	
;----------------------------END RECORD

;NPC RECORD $80
	.TF     DATA.TLK.L001.80.BIN,BIN
	
REC80

;TALK: INTRODUCTION
	.AZ -/$I.Hi, I have the default converstion text. There are five people in town with custom conversations. Everyone else will say what I say./,#$8D,#$8D,/You can ask me things like name, work, or other keywords in my responses/,#$8D,#$8D,/Press tab to toggle voice mode./

;TALK: NAME  
	.AZ -/$!NAME:$N.My name is Ffred Krautcabin/

;TALK: JOB
	.AZ -/$!JOB:$N.To introduce NPC conversation mode in Nox Archaist/

;TALK: CONVERSATION
	.AZ -/$!CONV:$N.We're having a convesation right now. Isn't it fun? :-)/,#$8D,#$8D,/Try asking me about "Grumblebottom" in each voice mode (normal-whisper-yell)/

	
;TALK: NPC
	.AZ -/$!NPC:$N.We're having a convesation right now. Isn't it fun? :-)/,#$8D,#$8D,/Try asking me about "Grumblebottom" in each voice mode (normal-whisper-yell)/

;TALK: GRUMBLEBOTTOM (YELL)
	.AZ -/$!GRUM:$Y.Why are you yelling? The whole town can hear you!"/
	
;TALK: RAISTLIN (WHISPER)
	.AZ -/$!GRUM:$W.Ah, Grumblebottom. He is a grumpy dwarf that hangs about near the town hall around 10am./,#$8D,#$8D,/To change the time using playtest key shift+9 then enter 1000. You can set other times too using 24 hour format/
	
;TALK: RAISTLIN (NORMAL)
	.AZ -/$!GRUM.Who? I don't know what you are talking about/

;TALK: TOWN HALL
	.AZ -/$!TOW:$N.The town hall is the building directly north of the town entrance./,#$8D,#$8D,/Grumblebottom hangs out a little west of the town hall entrance./
		
	
;TALK: DEFAULT (YELL)
	.AZ -/$!DFL:$Y.Why are you yelling?/

;TALK: DEFAULT (WHISPER)
	.AZ -/$!DFL:$W.I can't hear you, what did you say?/

;TALK: DEFAULT (NORMAL)
	.AZ -/$!DFL:$K.I don't know anything about your $K./
	
;TALK: BYE
	.AZ -/$!BYE.Bye. Enjoy our test town./

	
;END OF TALK RECORD
	.BS $1,$FC
	
;----------------------------END RECORD

;NPC RECORD $88
	.TF     DATA.TLK.L001.88.BIN,BIN
	
REC88

;TALK: INTRODUCTION
	.AZ -/$I.Hi, I have the default converstion text. There are five people in town with custom conversations. Everyone else will say what I say./,#$8D,#$8D,/You can ask me things like name, work, or other keywords in my responses/,#$8D,#$8D,/Press tab to toggle voice mode./

;TALK: NAME  
	.AZ -/$!NAME:$N.My name is Ffred Krautcabin/

;TALK: JOB
	.AZ -/$!JOB:$N.To introduce NPC conversation mode in Nox Archaist/

;TALK: CONVERSATION
	.AZ -/$!CONV:$N.We're having a convesation right now. Isn't it fun? :-)/,#$8D,#$8D,/Try asking me about "Grumblebottom" in each voice mode (normal-whisper-yell)/

	
;TALK: NPC
	.AZ -/$!NPC:$N.We're having a convesation right now. Isn't it fun? :-)/,#$8D,#$8D,/Try asking me about "Grumblebottom" in each voice mode (normal-whisper-yell)/

;TALK: GRUMBLEBOTTOM (YELL)
	.AZ -/$!GRUM:$Y.Why are you yelling? The whole town can hear you!"/
	
;TALK: RAISTLIN (WHISPER)
	.AZ -/$!GRUM:$W.Ah, Grumblebottom. He is a grumpy dwarf that hangs about near the town hall around 10am./,#$8D,#$8D,/To change the time using playtest key shift+9 then enter 1000. You can set other times too using 24 hour format/
	
;TALK: RAISTLIN (NORMAL)
	.AZ -/$!GRUM.Who? I don't know what you are talking about/

;TALK: TOWN HALL
	.AZ -/$!TOW:$N.The town hall is the building directly north of the town entrance./,#$8D,#$8D,/Grumblebottom hangs out a little west of the town hall entrance./
		
	
;TALK: DEFAULT (YELL)
	.AZ -/$!DFL:$Y.Why are you yelling?/

;TALK: DEFAULT (WHISPER)
	.AZ -/$!DFL:$W.I can't hear you, what did you say?/

;TALK: DEFAULT (NORMAL)
	.AZ -/$!DFL:$K.I don't know anything about your $K./
	
;TALK: BYE
	.AZ -/$!BYE.Bye. Enjoy our test town./

	
;END OF TALK RECORD
	.BS $1,$FC
	
;----------------------------END RECORD

;NPC RECORD $90
	.TF     DATA.TLK.L001.90.BIN,BIN
	
REC90

;TALK: INTRODUCTION
	.AZ -/$I.Hi, I have the default converstion text. There are five people in town with custom conversations. Everyone else will say what I say./,#$8D,#$8D,/You can ask me things like name, work, or other keywords in my responses/,#$8D,#$8D,/Press tab to toggle voice mode./

;TALK: NAME  
	.AZ -/$!NAME:$N.My name is Ffred Krautcabin/

;TALK: JOB
	.AZ -/$!JOB:$N.To introduce NPC conversation mode in Nox Archaist/

;TALK: CONVERSATION
	.AZ -/$!CONV:$N.We're having a convesation right now. Isn't it fun? :-)/,#$8D,#$8D,/Try asking me about "Grumblebottom" in each voice mode (normal-whisper-yell)/

	
;TALK: NPC
	.AZ -/$!NPC:$N.We're having a convesation right now. Isn't it fun? :-)/,#$8D,#$8D,/Try asking me about "Grumblebottom" in each voice mode (normal-whisper-yell)/

;TALK: GRUMBLEBOTTOM (YELL)
	.AZ -/$!GRUM:$Y.Why are you yelling? The whole town can hear you!"/
	
;TALK: RAISTLIN (WHISPER)
	.AZ -/$!GRUM:$W.Ah, Grumblebottom. He is a grumpy dwarf that hangs about near the town hall around 10am./,#$8D,#$8D,/To change the time using playtest key shift+9 then enter 1000. You can set other times too using 24 hour format/
	
;TALK: RAISTLIN (NORMAL)
	.AZ -/$!GRUM.Who? I don't know what you are talking about/

;TALK: TOWN HALL
	.AZ -/$!TOW:$N.The town hall is the building directly north of the town entrance./,#$8D,#$8D,/Grumblebottom hangs out a little west of the town hall entrance./
		
	
;TALK: DEFAULT (YELL)
	.AZ -/$!DFL:$Y.Why are you yelling?/

;TALK: DEFAULT (WHISPER)
	.AZ -/$!DFL:$W.I can't hear you, what did you say?/

;TALK: DEFAULT (NORMAL)
	.AZ -/$!DFL:$K.I don't know anything about your $K./
	
;TALK: BYE
	.AZ -/$!BYE.Bye. Enjoy our test town./

	
;END OF TALK RECORD
	.BS $1,$FC
	
;----------------------------END RECORD

;NPC RECORD $98
	.TF     DATA.TLK.L001.98.BIN,BIN
	
REC98

;TALK: INTRODUCTION
	.AZ -/$I.Hi, I have the default converstion text. There are five people in town with custom conversations. Everyone else will say what I say./,#$8D,#$8D,/You can ask me things like name, work, or other keywords in my responses/,#$8D,#$8D,/Press tab to toggle voice mode./

;TALK: NAME  
	.AZ -/$!NAME:$N.My name is Ffred Krautcabin/

;TALK: JOB
	.AZ -/$!JOB:$N.To introduce NPC conversation mode in Nox Archaist/

;TALK: CONVERSATION
	.AZ -/$!CONV:$N.We're having a convesation right now. Isn't it fun? :-)/,#$8D,#$8D,/Try asking me about "Grumblebottom" in each voice mode (normal-whisper-yell)/

	
;TALK: NPC
	.AZ -/$!NPC:$N.We're having a convesation right now. Isn't it fun? :-)/,#$8D,#$8D,/Try asking me about "Grumblebottom" in each voice mode (normal-whisper-yell)/

;TALK: GRUMBLEBOTTOM (YELL)
	.AZ -/$!GRUM:$Y.Why are you yelling? The whole town can hear you!"/
	
;TALK: RAISTLIN (WHISPER)
	.AZ -/$!GRUM:$W.Ah, Grumblebottom. He is a grumpy dwarf that hangs about near the town hall around 10am./,#$8D,#$8D,/To change the time using playtest key shift+9 then enter 1000. You can set other times too using 24 hour format/
	
;TALK: RAISTLIN (NORMAL)
	.AZ -/$!GRUM.Who? I don't know what you are talking about/

;TALK: TOWN HALL
	.AZ -/$!TOW:$N.The town hall is the building directly north of the town entrance./,#$8D,#$8D,/Grumblebottom hangs out a little west of the town hall entrance./
		
	
;TALK: DEFAULT (YELL)
	.AZ -/$!DFL:$Y.Why are you yelling?/

;TALK: DEFAULT (WHISPER)
	.AZ -/$!DFL:$W.I can't hear you, what did you say?/

;TALK: DEFAULT (NORMAL)
	.AZ -/$!DFL:$K.I don't know anything about your $K./
	
;TALK: BYE
	.AZ -/$!BYE.Bye. Enjoy our test town./

	
;END OF TALK RECORD
	.BS $1,$FC
	
;----------------------------END RECORD

;NPC RECORD $A0
	.TF     DATA.TLK.L001.A0.BIN,BIN
	
RECA0 ;npc who terminates
; ;TALK: INTRODUCTION
	; .AZ -/$I:$T/,#$80,/.You see a grumpy old man harumphing about./

;TALK: INTRODUCTION
	.AZ -/$I.You see a grumpy old man harumphing about./

	
;TALK: NAME  
	.AZ -/$!NAME.None of your business./

;TALK: JOB
	.AZ -/$!JOB:$T/,#$80,/.Not to stand here talking' to you, that's for sure!./

;--------

;TALK: DEFAULT (NORMAL)
	.AZ -/$!DFL.Quit wasting my time./

;TALK: END (SUBCOMMAND TRIGGERED)
	.AZ -/$!%END.Bugger off!./

;TALK: BYE
	.AZ -/$!BYE.Bye/

;END OF TALK RECORD
	.BS $1,$FC
;----------------------------END RECORD

;NPC RECORD $A8
	.TF     DATA.TLK.L001.A8.BIN,BIN
	
RECA8 ;town wizard
;TALK: INTRODUCTION
	.AZ -/$I.You see a tall wizard who is apprehensively fiddling with a wand/

;TALK: NAME  
	.AZ -/$!NAME.I am Ojithar. What business is it of yours?/

;TALK: JOB
	.AZ -/$!JOB.Begone! I have no time for your foolery./
	
;TALK: RUIN (NORMAL)
	.AZ -/$!RUIN:$H/,#$13,#$11,#$22,#$00,/.Ah, the ancient ruin of Blavvar. A great evil is said to reside there. But, don't bother asking me for details until the rest of the game is written, because until then I have no idea :-)/

;TALK: RUIN (YELL)
	.AZ -/$!RUIN:$Y.I don't know what your talking about. If you want to ruin my day, your succeeding. And stop yelling. I have a headache./

;TALK: RUIN (WHISPER)
	.AZ -/$!RUIN:$W.Shhh, not here./
	
;TALK: RUIN (NORMAL)
	.AZ -/$!RUIN.Ruin what? You're ruining the air with your stench./
	
;TALK: DEFAULT (YELL)
	.AZ -/$!DFL:$Y.Why are you yelling? You'll wake the dead./

;TALK: DEFAULT (WHISPER)
	.AZ -/$!DFL:$W.What's that you say? Speak up!/

;TALK: DEFAULT (NORMAL)
	.AZ -/$!DFL.I don't have time for your foolery./

;TALK: BYE
	.AZ -/$!BYE.I'm busy, it's best you be going./
	
;END OF TALK RECORD
	.BS $1,$FC
;----------------------------END RECORD

;NPC RECORD $B0
	.TF     DATA.TLK.L001.B0.BIN,BIN
	
RECB0

;TALK: INTRODUCTION
	.AZ -/$I.Hi, I have the default converstion text. There are five people in town with custom conversations. Everyone else will say what I say./,#$8D,#$8D,/You can ask me things like name, work, or other keywords in my responses/,#$8D,#$8D,/Press tab to toggle voice mode./

;TALK: NAME  
	.AZ -/$!NAME:$N.My name is Ffred Krautcabin/

;TALK: JOB
	.AZ -/$!JOB:$N.To introduce NPC conversation mode in Nox Archaist/

;TALK: CONVERSATION
	.AZ -/$!CONV:$N.We're having a convesation right now. Isn't it fun? :-)/,#$8D,#$8D,/Try asking me about "Grumblebottom" in each voice mode (normal-whisper-yell)/

	
;TALK: NPC
	.AZ -/$!NPC:$N.We're having a convesation right now. Isn't it fun? :-)/,#$8D,#$8D,/Try asking me about "Grumblebottom" in each voice mode (normal-whisper-yell)/

;TALK: GRUMBLEBOTTOM (YELL)
	.AZ -/$!GRUM:$Y.Why are you yelling? The whole town can hear you!"/
	
;TALK: RAISTLIN (WHISPER)
	.AZ -/$!GRUM:$W.Ah, Grumblebottom. He is a grumpy dwarf that hangs about near the town hall around 10am./,#$8D,#$8D,/To change the time using playtest key shift+9 then enter 1000. You can set other times too using 24 hour format/
	
;TALK: RAISTLIN (NORMAL)
	.AZ -/$!GRUM.Who? I don't know what you are talking about/

;TALK: TOWN HALL
	.AZ -/$!TOW:$N.The town hall is the building directly north of the town entrance./,#$8D,#$8D,/Grumblebottom hangs out a little west of the town hall entrance./
		
	
;TALK: DEFAULT (YELL)
	.AZ -/$!DFL:$Y.Why are you yelling?/

;TALK: DEFAULT (WHISPER)
	.AZ -/$!DFL:$W.I can't hear you, what did you say?/

;TALK: DEFAULT (NORMAL)
	.AZ -/$!DFL:$K.I don't know anything about your $K./
	
;TALK: BYE
	.AZ -/$!BYE.Bye. Enjoy our test town./

	
;END OF TALK RECORD
	.BS $1,$FC
	
;----------------------------END RECORD
;NPC RECORD $B8
	.TF     DATA.TLK.L001.B8.BIN,BIN
	
RECB8 ;grumpy dwarf	
;TALK: INTRODUCTION
	.AZ -/$I.You see a stocky dwarf with a beard larger than most barrels of ale./

;TALK: NAME  
	.AZ -/$!NAME.They call me Grumblebottom/

;TALK: JOB
	.AZ -/$!JOB.I am a blacksmith and an adventurer./

;TALK: ADVENTURER
	.AZ -/$!ADVE.Aye. I've heard rumors of a great treasure, and great evil, in some old ruins to the northwest of here./

;TALK: EVIL
	.AZ -/$!EVIL.Not sure 'bout that. I'm going to talk to Ojithar and see what he knows/

;TALK: OJITHAR (YELL)
	.AZ -/$!OJIT.He's the town wizard. Hey - you're asking a lot 'o questions. Maybe I should just run 'ye through?/

	
;------

;TALK: DEFAULT (YELL)
	.AZ -/$!DFL:$Y.What are 'ye yelling 'fer?/

;TALK: DEFAULT (NORMAL)
	.AZ -/$!DFL:$K.I don't know anything about $K./

;TALK: CURSE (SUBCOMMAND TRIGGERED)
	.AZ -/$!%CRS:$K.The last person who said $K to me ended up on the business end 'o me axe!/

;TALK: BYE
	.AZ -/$!BYE.I was done talkin' to 'ye anyway./

	
;END OF TALK RECORD

	.BS $1,$FC

;----------------------------END RECORD


;============================
;============================END FILE
;============================
;============================
	
;END OF TALK FILE
	.BS $1,$FF
	
	; .NO $BFFF,$00	;FILLER TO THE UPPER LIMIT OF OF THE MEMORY ALLOCATED FOR THIS DATA FILE 
					; ;This enables SBASM to generate an error if/when the code overshoots this memory address.
					; ;It also kees this target file at a fixed size so the track/sector stays the same on the disk image, which is needed to load the file via MY.RWTS
