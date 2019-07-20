" Vim syntax file
" Language: GNU x86 Assembler with INTEL syntax



"""""""""""""""""""""""""
""" SET UP NEW SYNTAX """
"""""""""""""""""""""""""


" Clear old syntax settings
if version < 600
	syn clear
elseif exists("b:current_syntax")
	finish
endif


" assembly is case insensitive
syn case ignore



""""""""""""""""""""""""""""
""" DEFINE SYNTAX GROUPS """
""""""""""""""""""""""""""""


""" COMMENTS

" EOL comments start with # and go to end of line
" Multi-line comments start with /* and end with */
syn region asmComment  start="#"   keepend end="$"     contains=asmTodo
syn region asmComment  start="/\*"         end="\*/"   contains=asmTodo

" Make a syntax group for todos, fixmes, etc. within comments
syn keyword asmTodo contained TODO NOTE FIXME ERROR XXX


""" LABELS

" Labels for us must start with a period, underscore or letter, and then
" subsequently contain only underscores, letters, and numbers. Label
" definitions end in a colon, while label references don't. These are treated
" differently.
syn match asmIdentifier "\.\=\<[a-z_][a-z0-9_]*\>" contains=@asmDirective
syn match asmLabel      "\.\=\<[a-z_][a-z0-9_]*\>:"he=e-1


""" DIRECTIVES

" standard directives for data types
syn match asmType "\.\<ascii\>"
syn match asmType "\.\<asciz\>"
syn match asmType "\.\<byte\>"
syn match asmType "\.\<double\>"
syn match asmType "\.\<float\>"
syn match asmType "\.\<hword\>"
syn match asmType "\.\<int\>"
syn match asmType "\.\<long\>"
syn match asmType "\.\<octa\>"
syn match asmType "\.\<quad\>"
syn match asmType "\.\<short\>"
syn match asmType "\.\<single\>"
syn match asmType "\.\<sleb128\>"
syn match asmType "\.\<string\(8\|16\|32\|64\)\=\>"
syn match asmType "\.\<uleb128\>"
syn match asmType "\.\<word\>"

" standard preprocessing directives
syn match asmPreProc "\.\<if\>"
syn match asmPreProc "\.\<else\>"
syn match asmPreProc "\.\<elseif\>"
syn match asmPreProc "\.\<endif\>"
syn match asmPreProc "\.\<ifdef\>"
syn match asmPreProc "\.\<ifndef\>"
syn match asmPreProc "\.\<ifnotdef\>"

" standard macro directives
syn match asmMacro "\.\<altmacro\>"
syn match asmMacro "\.\<noaltmacro\>"
syn match asmMacro "\.\<macro\>"
syn match asmMacro "\.\<endm\>"
syn match asmMacro "\.\<exitm\>"
syn match asmMacro "\.\<purgem\>"

" standard include directive
syn match asmInclude "\.\<include\>"
syn match asmInclude "\.\<incbin\>"

" standard debug directives
syn match asmDbg "\.\<cfi_sections\>"
syn match asmDbg "\.\<cfi_startproc\>"
syn match asmDbg "\.\<cfi_endproc\>"
syn match asmDbg "\.\<cfi_personality\>"
syn match asmDbg "\.\<cfi_lsda\>"
syn match asmDbg "\.\<cfi_def_cfa\>"
syn match asmDbg "\.\<cfi_def_cfa_register\>"
syn match asmDbg "\.\<cfi_def_cfa_offset\>"
syn match asmDbg "\.\<cfi_adjust_cfa_offset\>"
syn match asmDbg "\.\<cfi_offset\>"
syn match asmDbg "\.\<cfi_rel_offset\>"
syn match asmDbg "\.\<cfi_register\>"
syn match asmDbg "\.\<cfi_restore\>"
syn match asmDbg "\.\<cfi_undefined\>"
syn match asmDbg "\.\<cfi_same_value\>"
syn match asmDbg "\.\<cfi_remember_state\>"
syn match asmDbg "\.\<cfi_return_column\>"
syn match asmDbg "\.\<cfi_signal_frame\>"
syn match asmDbg "\.\<cfi_window_save\>"
syn match asmDbg "\.\<cfi_escape\>"
syn match asmDbg "\.\<cfi_val_encoded_addr\>"
syn match asmDbg "\.\<def\>"
syn match asmDbg "\.\<dim\>"
syn match asmDbg "\.\<endef\>"
syn match asmDbg "\.\<func\>"
syn match asmDbg "\.\<endfunc\>"
syn match asmDbg "\.\<loc\>"
syn match asmDbg "\.\<loc_mark_labels\>"
syn match asmDbg "\.\<scl\>"
syn match asmDbg "\.\<size\>"
syn match asmDbg "\.\<stab[dns]\>"
syn match asmDbg "\.\<tag\>"

" the rest of the standard GAS directives
syn match asmStdDirective "\.\<abort\>"
syn match asmStdDirective "\.\<align\>"
syn match asmStdDirective "\.\<app-file\>"
syn match asmStdDirective "\.\<arch\>"
syn match asmStdDirective "\.\<balign\>"
syn match asmStdDirective "\.\<bundle_align\>"
syn match asmStdDirective "\.\<bundle_lock\>"
syn match asmStdDirective "\.\<bundle_unlock\>"
syn match asmStdDirective "\.\<comm\>"
syn match asmStdDirective "\.\<data\>"
syn match asmStdDirective "\.\<desc\>"
syn match asmStdDirective "\.\<eject\>"
syn match asmStdDirective "\.\<end\>"
syn match asmStdDirective "\.\<equ\>"
syn match asmStdDirective "\.\<equiv\>"
syn match asmStdDirective "\.\<eqv\>"
syn match asmStdDirective "\.\<err\>"
syn match asmStdDirective "\.\<error\>"
syn match asmStdDirective "\.\<extern\>"
syn match asmStdDirective "\.\<fail\>"
syn match asmStdDirective "\.\<file\>"
syn match asmStdDirective "\.\<fill\>"
syn match asmStdDirective "\.\<globa\=l\>"
syn match asmStdDirective "\.\<gnu_attribute\>"
syn match asmStdDirective "\.\<hidden\>"
syn match asmStdDirective "\.\<ident\>"
syn match asmStdDirective "\.\<internal\>"
syn match asmStdDirective "\.\<irp\>"
syn match asmStdDirective "\.\<irpc\>"
syn match asmStdDirective "\.\<lcomm\>"
syn match asmStdDirective "\.\<lflags\>"
syn match asmStdDirective "\.\<line\>"
syn match asmStdDirective "\.\<linkonce\>"
syn match asmStdDirective "\.\<literal_position\>"
syn match asmStdDirective "\.\<ln\>"
syn match asmStdDirective "\.\<list\>"
syn match asmStdDirective "\.\<local\>"
syn match asmStdDirective "\.\<mri\>"
syn match asmStdDirective "\.\<nolist\>"
syn match asmStdDirective "\.\<offset\>"
syn match asmStdDirective "\.\<org\>"
syn match asmStdDirective "\.\<p2align[wl]\>"
syn match asmStdDirective "\.\<popsection\>"
syn match asmStdDirective "\.\<previous\>"
syn match asmStdDirective "\.\<print\>"
syn match asmStdDirective "\.\<protected\>"
syn match asmStdDirective "\.\<psize\>"
syn match asmStdDirective "\.\<pushsection\>"
syn match asmStdDirective "\.\<reloc\>"
syn match asmStdDirective "\.\<rept\>"
syn match asmStdDirective "\.\<sbttl\>"
syn match asmStdDirective "\.\<section\>"
syn match asmStdDirective "\.\<set\>"
syn match asmStdDirective "\.\<skip\>"
syn match asmStdDirective "\.\<space\>"
syn match asmStdDirective "\.\<struct\>"
syn match asmStdDirective "\.\<subsection\>"
syn match asmStdDirective "\.\<symver\>"
syn match asmStdDirective "\.\<text\>"
syn match asmStdDirective "\.\<title\>"
syn match asmStdDirective "\.\<type\>"
syn match asmStdDirective "\.\<val\>"
syn match asmStdDirective "\.\<version\>"
syn match asmStdDirective "\.\<vtable_entry\>"
syn match asmStdDirective "\.\<vtable_inherit\>"
syn match asmStdDirective "\.\<warning\>"
syn match asmStdDirective "\.\<weak\>"
syn match asmStdDirective "\.\<weakref\>"
syn match asmStdDirective "\.\<\(att\|intel\)_\(syntax\|mnemonic\)\>"

syn cluster asmDirective contains=asmStdDirective,asmType,asmPreProc,asmMacro,asmInclude,asmDbg



""" CONSTANTS

" strings and characters
syn match   asmSpecial   display contained "\\\(x\x\+\|\o\{1,3}\|.\|$\)"
syn region  asmString    start=+"+ skip=+\\"+ end=+"+ oneline contains=asmSpecial
syn match   asmCharacter "'\(\(\\\(x\x\+\|\o\{1,3}\|.\)\)\|[^\\]\)'" contains=asmSpecial

" binary integers
syn match   asmBinNumber "\<0b[0-1]\+\>"
syn match   asmBinNumber "\<[0-1]\+b\>"

" octal integers
syn match   asmOctNumber "\<0\o\+\>"
syn match   asmOctNumber "\<0[oq]\o\+\>"
syn match   asmOctNumber "\<\o\+[oq]\>"

" decimal integers
syn match   asmDecNumber "\<\d\+\>"

" hexadecimal integers
syn match   asmHexNumber "\<0x\x\+\>"
syn match   asmHexNumber "\<\x\+h\>"

" floating point numbers
syn match   asmFltNumber "\<\d\+\.\d*\(e[+-]\=\d\+\)\=\>"
syn keyword asmFltNumber Inf Infinity Indefinite NaN SNaN QNaN


""" REGISTERS

" general purpose registers (rax, rbx, rcx, rdx, rbp, rsp, rdi, rsi, r8-15)
syn match asmGenRegister "\<[a-d][hlx]\>"
syn match asmGenRegister "\<[er][a-d]x\>"
syn match asmGenRegister "\<[bs]pl\=\>"
syn match asmGenRegister "\<[er][sb]p\>"
syn match asmGenRegister "\<[ds]il\=\>"
syn match asmGenRegister "\<[er][ds]i\>"
syn match asmGenRegister "\<r\([89]\|1[0-5]\)[bwd]\=\>"

" instruction pointer
syn match asmPcRegister  "\<[er]\=ip\>"

" segment registers
syn match asmSegRegister "\<[c-gs]s\>"

" floating point registers
syn match asmFpuRegister "\<st\o\>"

" MMX registers
syn match asmMmxRegister "\<mm\o\>"

" vector registers
syn match asmVecRegister "\<[xyz]mm\(\d\|[12]\d\|3[01]\)\>"

" control registers
syn match asmCtrlRegister "\<cr[0-8]\>"

" debug registers
syn match asmDbgRegister "\<dr\o\>"


""" DATA SIZES

syn keyword asmDataSize byte word dword qword xmmword ymmword zmmword ptr far near short rel


""" INSTRUCTIONS

" standard instructions (conditionals)
syn match   asmStdIns "\<\(cmov\|j\|set\)\(n\=\([abgl]e\=\|[ceosz]\)\|p[eo]\=\)\>"
" standard instructions (not conditional, in alphabetical order)
syn keyword asmStdIns aaa aad aam aas adc add and
syn keyword asmStdIns bound bsf bsr bswap bt btc btr bts
syn keyword asmStdIns call cbw cdq cdqe clc cld cmc cmp cmps cmpsb cmpsd cmpsq cmpsw
syn keyword asmStdIns cmpxchg cpuid cqo cwd cwde
syn keyword asmStdIns daa das dec div enter
syn keyword asmStdIns idiv imul inc iret iretd iretw iretq
syn keyword asmStdIns jcxz jecxz jmp jrcxz
syn keyword asmStdIns lahf lds lea leave les lfs lgs lodsb lodsd lodsq lodsw loop loope 
syn keyword asmStdIns loopne lss
syn keyword asmStdIns mov movs movsb movsd movsq movsx movsxd movsw movzx mul
syn keyword asmStdIns neg nop not or pause pop popa popad popf popfd popfq push pusha pushad
syn keyword asmStdIns pushf pushfd pushfq
syn keyword asmStdIns rcl rcr rep repe repne repnz repz ret rol ror 
syn keyword asmStdIns sahf sal sar sbb scas scasb scasd scasw shl shld shr shrd stc std
syn keyword asmStdIns stos stosb stosd stosq stosw sub
syn keyword asmStdIns test ud0 ud1 ud2 xadd xchg xlatb xor 

" System instructions
syn match   asmSysIns "\<mov\(cr[0-8]\|dr\o\)\>"
syn keyword asmSysIns arpl clgi cli clts hlt in insb insd insw int into int1 int3 invd 
syn keyword asmSysIns invlpg invpcid lar lgdt lidt lldt lmsw lock lsl ltr out outsb outsd
syn keyword asmSysIns outsw rdpmc rdtsc rsm sgdt sidt sldt smsw sti str syscall sysret verr
syn keyword asmSysIns verw wait wbinvd 

" Extended instructions (require certain cpuid bits be set)
syn keyword asmExtIns adcx adox aesdec aesdeclast aesenc aesenclast aesimc aeskeygenassist
syn keyword asmExtIns andn bextr blcfill blci blcic blcmsk blcs blsfill blsi blsic blsr
syn keyword asmExtIns bndcl bndcn bndcu bndldx bndmk bndmov bndstx bslmsk bzhi
syn keyword asmExtIns clac cldemote clflush clflushopt clwb cmpxchg8b cmpxchg16b crc32
syn keyword asmExtIns fxrstor fxsave gf2p8affineinvqb gf2p8affineqb gf2p8mulb lzcnt monitor
syn keyword asmExtIns movbe movdir64b movdiri mulx mwait pdep pext popcnt ptwrite rdfsbase
syn keyword asmExtIns rdgsbase rdmsr rdpid rdpkru rdrand rdseed rdtscp rorx sarx shrx shlx 
syn keyword asmExtIns sha1msg1 sha1msg2 sha1nexte sha1rnds4 sha256msg1 sha256msg2
syn keyword asmExtIns sha256rnds2 stac swapgs sysenter sysexit t1mskc tpause tzcnt tzmsk 
syn keyword asmExtIns umonitor umwait wrfsbase wrgsbase wrmsr wrpkru xabort xacquire 
syn keyword asmExtIns xrelease xbegin xend xgetbv xrstor xrstors xsave xsavec xsaveopt
syn keyword asmExtIns xsaves xsetbv xtest 

" Floating point instructions
syn match   asmFpuIns "\<fcmovn\=\(b\=e\=\|u\)\>"
syn keyword asmFpuIns f2xm1 fabs fadd faddp fbld fbstp fchs fclex fcom fcomi fcomip fcomp
syn keyword asmFpuIns fcompp fcos fdecstp fdiv fdivp fdivr fdivrp ffree fiadd ficom fidiv
syn keyword asmFpuIns fidivr fild fimul fincstp finit fist fistp fisttp fisub fisubr fld
syn keyword asmFpuIns fld1 fldcw fldenv fldl2e fldl2t fldlg2 fldln2 fldpi fldz fmul fmulp
syn keyword asmFpuIns fnclex fninit fnop fnsave fnstcw fnstenv fnstsw fpatan fprem fprem1
syn keyword asmFpuIns fptan frndint frstor fsave fscale fsin fsincos fsqrt fst fstcw fstenv
syn keyword asmFpuIns fstp fstsw fsub fsubp fsubr fsubrp ftst fucom fucomi fucomip fucomp
syn keyword asmFpuIns fucompp fwait fxam fxch fxtract fyl2x fyl2xp1

" MMX instructions
syn keyword asmMmxIns emms maskmovq pshufw
 
" SSE instructions
syn keyword asmSseIns addpd addps addsd addss addsubpd addsubps andnpd andnps andpd andps
syn keyword asmSseIns blendpd blendps blendvpd blendvps cmppd cmpps cmpss comisd comiss
syn keyword asmSseIns cvtdq2pd cvtdq2ps cvtpd2dq cvtpd2pi cvtpd2ps cvtpi2pd cvtpi2ps
syn keyword asmSseIns cvtps2dq cvtps2pd cvtps2pi cvtsd2si cvtsd2ss cvtsi2sd cvtsi2ss 
syn keyword asmSseIns cvtss2sd cvtss2si cvttpd2dq cvttpd2pi cvttps2dq cvttps2pi cvttsd2si
syn keyword asmSseIns cvttss2si divpd divps divsd divss dppd dpps extractps haddpd haddps
syn keyword asmSseIns hsubpd hsubps insertps lfence maskmovdqu maxpd maxps maxsd maxss
syn keyword asmSseIns mfence minpd minps minsd minss movapd movaps movd movq movddup
syn keyword asmSseIns movdq2q movdqa movdqu movhlps movhpd movhps movlhps movlpd movlps
syn keyword asmSseIns movmskpd movmskps movntdq movntdqa movnti movntpd movntps movntq
syn keyword asmSseIns movq2dq movshdup movsldup movss movupd movups mpsadbw mulpd mulps
syn keyword asmSseIns mulsd mulss orpd orps pabsb pabsd pabsq pabsw packsswb packssdw
syn keyword asmSseIns packusdw packuswb paddb paddd paddq paddw paddsb paddsw paddusb 
syn keyword asmSseIns paddusw palignr pand pandn pavgb pavgw pblendvb pblendw pclmulqdq
syn keyword asmSseIns pcmpeqb pcmpeqd pcmpeqw pcmpeqq pcmpestri pcmpestrm pcmpgtb pcmpgtd
syn keyword asmSseIns pcmpgtw pcmpgtq pcmpistri pcmpistrm pextrb pextrd pextrq pextrw
syn keyword asmSseIns phaddd phaddw phaddsw phminposuw phsubd phsubw phsubsw pinsrb pinsrd
syn keyword asmSseIns pinsrq pinsrw pmaddubsw pmaddwd pmaxsb pmaxsd pmaxsq pmaxsw pmaxub
syn keyword asmSseIns pmaxuw pmaxud pmaxuq pminsb pminsw pminsd pminsq pminub pminuw pminud
syn keyword asmSseIns pminuq pmovmskb pmovsxbw pmovsxbd pmovsxbq pmovsxwd pmovsxwq pmovsxdq
syn keyword asmSseIns pmovzxbw pmovzxbd pmovzxbq pmovzxwd pmovzxwq pmovzxdq pmuldq
syn keyword asmSseIns pmulhrsw pmulhuw pmulhw pmulld pmullq pmullw pmuludq por prefetchw
syn keyword asmSseIns prefetch0 prefetch1 prefetch2 prefetchnta psadbw pshufb pshufd pshufw
syn keyword asmSseIns pshuflw psignb psignw psignd psllw pslld psllq pslldq psraw psrad 
syn keyword asmSseIns psraq psrlw psrld psrlq psrldq psubb psubw psubd psubq psubsb psubsw
syn keyword asmSseIns psubusb psubusw ptest punpckhbw punpckhqd punpckhdq punpckhqdq
syn keyword asmSseIns punpcklbw punpcklwd punpckldq punpcklqdq pxor rcpps rcpss roundpd
syn keyword asmSseIns roundps roundsd roundss rsqrtps rsqrtss sfence shufpd shufps sqrtpd
syn keyword asmSseIns sqrtps sqrtsd sqrtss stmxcsr subpd subps subsd subss ucomisd ucomiss
syn keyword asmSseIns unpckhpd unpckhps unpcklpd unpcklps xorpd xorps

" AVX instructions
syn keyword asmAvxIns kaddb kaddd kaddq kaddw kandb kandd kandq kandw kandnb kandnd kandnq
syn keyword asmAvxIns kandnw kmovb kmovd kmovq kmovw knotb knotd knotq knotw korb kord korq
syn keyword asmAvxIns korw kortestb kortestd kortestq kortestw kshiftlb kshiftld kshiftlq
syn keyword asmAvxIns kshiftlw kshiftrb kshiftrd kshiftrq kshiftrw ktestb ktestd ktestq
syn keyword asmAvxIns ktestw kunpckbw kunpckwd kunpckdq kxnorb kxnord kxnorq kxnorw kxorb
syn keyword asmAvxIns kxord kxorq kxorw
syn keyword asmAvxIns vaddpd vaddps vaddsd vaddss vaddsubd vaddsubps vaesdec vaesdeclast
syn keyword asmAvxIns vaesenc vaesenclast vaesimc vaeskeygenassist vandnpd vandnps vandpd
syn keyword asmAvxIns vandps vblendpd vblendps vblendvpd vblendvps vcmppd vcmpps vcmpsd
syn keyword asmAvxIns vcmpss vcomisd vcomiss vcvtdq2pd vcvtdq2ps vcvtpd2dq vcvtpd2pi
syn keyword asmAvxIns vcvtpd2ps vcvtpi2pd vcvtpi2ps vcvtps2dq vcvtps2pd vcvtps2pi vcvtsd2si
syn keyword asmAvxIns vcvtsd2ss vcvtsi2sd vcvtsi2ss vcvtss2sd vcvtss2si vcvttpd2dq 
syn keyword asmAvxIns vcvttpd2pi vcvttps2dq vcvttps2pi vcvttsd2si vcvttss2si vdivpd vdivps
syn keyword asmAvxIns vdivsd vdivss vdppd vdpps vextractps vhaddpd vhaddps vhsubpd vhsubps
syn keyword asmAvxIns vinsertps vmaskmovdqu vmaxpd vmaxps vmaxsd vmaxss vminpd vminps vminsd
syn keyword asmAvxIns vminss vmovapd vmovaps vmovd vmovq vmovddup vmovdqa vmovdqa32 
syn keyword asmAvxIns vmovdqa64 vmovdqu vmovdqu8 vmovdqu16 vmovdqu32 vmovdqu64 vmovhlps
syn keyword asmAvxIns vmovhpd vmovhps vmovlhps vmovlpd vmovlps vmovmskpd movmskps vmovntdq
syn keyword asmAvxIns vmovntdqa vmovntpd vmovntps vmovshdup vmovsldup vmovss vmovupd vmovups
syn keyword asmAvxIns vmpsadbw vmulpd vmulps vmulsd vmulss vorpd vorps vpabsb vpabsd vpabsq
syn keyword asmAvxIns vpabsw vpacksswb vpackssdw vpackusdw vpackuswb vpaddb vpaddd vpaddq 
syn keyword asmAvxIns vpaddw vpaddsb vpaddsw vpaddusb vpaddusw vpalignr vpand vpandd vpandq
syn keyword asmAvxIns vpandn vpandnd vpandnq vpavgb vpavgw vpblendvb vpblendw vpclmulqdq
syn keyword asmAvxIns vpcmpeqb vpcmpeqd vpcmpeqw vpcmpeqq vpcmpestri vpcmpestrm vpcmpgtb 
syn keyword asmAvxIns vpcmpgtd vpcmpgtw vpcmpgtq vpcmpistri vpcmpistrm vpextrb vpextrd 
syn keyword asmAvxIns vpextrq vpextrw vphaddd vphaddw vphaddsw vphminposuw vphsubd vphsubw
syn keyword asmAvxIns vphsubsw vpinsrb vpinsrd vpinsrq vpinsrw vpmaddubsw vpmaddwd vpmaxsb 
syn keyword asmAvxIns vpmaxsd vpmaxsq vpmaxsw vpmaxub vpmaxuw vpmaxud vpmaxuq vpminsb 
syn keyword asmAvxIns vpminsw vpminsd vpminsq vpminub vpminuw vpminud vpminuq vpmovmskb
syn keyword asmAvxIns vpmovsxbw vpmovsxbd vpmovsxbq vpmovsxwd vpmovsxwq vpmovsxdq
syn keyword asmAvxIns vpmovzxbw vpmovzxbd vpmovzxbq vpmovzxwd vpmovzxwq vpmovzxdq vpmuldq
syn keyword asmAvxIns vpmulhrsw vpmulhuw vpmulhw vpmulld vpmullq vpmullw vpmuludq vpor
syn keyword asmAvxIns vpord vporq vpsadbw vpshufb vpshufd vpshufw vpshuflw vpsignb vpsignw 
syn keyword asmAvxIns vpsignd vpsllw vpslld vpsllq vpslldq vpsraw vpsrad vpsraq vpsrlw 
syn keyword asmAvxIns vpsrld vpsrlq vpsrldq vpsubb vpsubw vpsubd vpsubq vpsubsb vpsubsw
syn keyword asmAvxIns vpsubusb vpsubusw vptest vpunpckhbw vpunpckhqd vpunpckhdq vpunpckhqdq
syn keyword asmAvxIns vpunpcklbw vpunpcklwd vpunpckldq vpunpcklqdq vpxor vpxord vpxorq
syn keyword asmAvxIns vrcpps vrcpss vroundpd vroundps vroundsd vroundss vrsqrtps vrsqrtss
syn keyword asmAvxIns vshufpd vshufps vsqrtpd vsqrtps vsqrtsd vsqrtss vstmxcsr vsubpd vsubps
syn keyword asmAvxIns vsubsd vsubss vucomisd vucomiss vunpckhpd vunpckhps vunpcklpd 
syn keyword asmAvxIns vunpcklps vxorpd vxorps
syn keyword asmAvxIns valignd valignq vblenddmpd vblenddmps vbroadcastss vbroadcastsd
syn keyword asmAvxIns vbroadcastf128 vbroadcastf32x2 vbroadcastf32x4 vbroadcastf64x2 
syn keyword asmAvxIns vbroadcastf32x8 vbroadcastf64x4 vcompresspd vcompressps vcvtpd2qq
syn keyword asmAvxIns vcvtpd2udq vcvtpd2uqq vcvtph2ps vcvtps2ph vcvtps2qq vcvtps2udq
syn keyword asmAvxIns vcvtps2uqq vcvtqq2pd vcvtqq2ps vcvtsd2usi vcvtss2usi vcvtudq2pd
syn keyword asmAvxIns vcvtudq2ps vcvtuqq2pd vcvtuqq2ps vcvtusi2sd vcvtusi2ss vdbpsadbw
syn keyword asmAvxIns vexpandpd vexpandps vextractf128 vextractf32x4 vextractf32x8
syn keyword asmAvxIns vextractf64x2 vextractf64x4 vextracti128 vextracti32x4 vextracti32x8
syn keyword asmAvxIns vextracti64x2 vextracti64x4 vfixupimmpd vfixupimmps vfixupimsd
syn keyword asmAvxIns vfixupimmss vfmadd132pd vfmadd213pd vfmadd231pd vfmadd132ps 
syn keyword asmAvxIns vfmadd213ps vfmadd231ps vfmadd132sd vfmadd213sd vfmadd231sd
syn keyword asmAvxIns vfmadd132ss vfmadd213ss vfmadd231ss vfmaddsub132pd vfmaddsub213pd 
syn keyword asmAvxIns vfmaddsub231pd vfmaddsub132ps vfmaddsub213ps vfmaddsub231ps 
syn keyword asmAvxIns vfmsub132pd vfmsub213pd vfmsub231pd vfmsub132ps vfmsub213ps 
syn keyword asmAvxIns vfmsub231ps vfmsub132sd vfmsub213sd vfmsub231sd vfmsub132ss 
syn keyword asmAvxIns vfmsub213ss vfmsub231ss vfmsubadd132pd vfmsubadd213pd vfmsubadd231pd
syn keyword asmAvxIns vfmsubadd132ps vfmsubadd213ps vfmsubadd231ps vfnmadd132pd vfnmadd213pd
syn keyword asmAvxIns vfnmadd231pd vfnmadd132ps vfnmadd213ps vfnmadd231ps vfnmadd132sd 
syn keyword asmAvxIns vfnmadd213sd vfnmadd231sd vfnmadd132ss vfnmadd213ss vfnmadd231ss
syn keyword asmAvxIns vfnmsub132pd vfnmsub213pd vfnmsub231pd vfnmsub132ps vfnmsub213ps 
syn keyword asmAvxIns vfnmsub231ps vfnmsub132sd vfnmsub213sd vfnmsub231sd vfnmsub132ss 
syn keyword asmAvxIns vfnmsub213ss vfnmsub231ss vfpclasspd vfpclassps vfpclasssd vfpclassss
syn keyword asmAvxIns vgatherdpd vgatherqpd vgatherdps vgatherqps vgetexppd vgetexpps
syn keyword asmAvxIns vgetexpsd vgetexpss vgetmantpd vgetmantps vgetmantsd vgetmantss
syn keyword asmAvxIns vinsertf128 vinsertf32x4 vinsertf64x2 vinsertf32x8 vinsertf64x4
syn keyword asmAvxIns vinserti128 vinserti32x4 vinserti64x2 vinserti32x8 vinserti64x4
syn keyword asmAvxIns vmaskmovps vmaskmovpd vpblendd vpblendmb vpblendmw vpblendmd vpblendmq
syn keyword asmAvxIns vpbroadcastb vpbroadcastw vpbroadcastd vpbroadcastq vbroadcasti32x2
syn keyword asmAvxIns vbroadcasti128 vbroadcasti32x4 vbroadcasti64x2 vbroadcasti32x8
syn keyword asmAvxIns vbroadcasti64x4 vpbroadcastmb2q vpbroadcastmw2d vpcmpb vpcmpub
syn keyword asmAvxIns vpcmpd vpcmpud vpcmpq vpcmpuq vpcmpw vpcmpuw vpcompressd vpcompressq
syn keyword asmAvxIns vpconflictd vpconflictq vperm2f128 vperm2i128 vpermb vpermd verpmw
syn keyword asmAvxIns vpermi2b vpermi2w vpermi2d vpermi2q vpermi2ps vpermi2pd vpermilpd
syn keyword asmAvxIns vpermilps vpermpd vpermps vpermq vpermt2b vpermt2d vpermt2pd vpermt2w
syn keyword asmAvxIns vpermt2q vpermt2ps vpexpandd vpexpandq vpgatherdd vpgatherqd 
syn keyword asmAvxIns vpgatherqq vplzcntd vplzcntq vpmadd52huq vpmadd52luq vpmaskmovd
syn keyword asmAvxIns vpmaskmovq vpmovb2m vpmovw2m vpmovd2m vpmovq2m vpmovdb vpmovsdb
syn keyword asmAvxIns vpmovusdb vpmovdw vpmovsdw vpmovusdw vpmovm2b vpmovm2w vpmovm2d
syn keyword asmAvxIns vpmovm2q vpmovqb vpmovsqb vpmovusqb vpmovqd vpmovsqd vpmovusqd vpmovqw
syn keyword asmAvxIns vpmovsqw vpmovusqw vpmovwb vpmovswb vpmovuswb vpmultishiftqb vprold
syn keyword asmAvxIns vprolvd vprolq vprolvq vprord vprorvd vprorq vprorvq vpscatterdd
syn keyword asmAvxIns vpscatterdq vpscatterqd vpscatterqq vpsllvw vpsllvd vpsllvq vpsravw
syn keyword asmAvxIns vpsravd vpsravq vpsrlvw vpsrlvd vpsrlvq vpternlogd vpternlogq
syn keyword asmAvxIns vptestmb vptestmw vptestmd vptestmq vptestnmb vptestnmw vptestnd
syn keyword asmAvxIns vptestnmq vrangepd vrangeps vrangesd vrangess vrcp14pd vrcp14ps
syn keyword asmAvxIns vrcp14sd vrcp14ss vreduceps vreducepd vreducesd vreducess vrndscalepd
syn keyword asmAvxIns vrndscaleps vrndscalesd vrndscaless vrsqrt14pd vrsqrt14ps vrsqrt14sd
syn keyword asmAvxIns vrsqrt14ss vscalefpd vscalefps vscalefsd vscalefss vscatterdpd
syn keyword asmAvxIns vscatterdps vscatterqpd vscatterqps vshuff32x4 vshuff64x2 vshufi32x4
syn keyword asmAvxIns vshufi64x2 vtestpd vtestps vzeroall vzeroupper

" SGX instructions
syn keyword asmSgxIns encls eadd eaug eblock ecreate edbgrd edbgwr eextend einit elbuc eldbc
syn keyword asmSgxIns eldb eldu emodpr emodt epa erdinfo eremove etrackc etrack ewb enclu
syn keyword asmSgxIns eacceptcopy eaccept eenter eexit egetkey emodpe ereport eresume enclv
syn keyword asmSgxIns edecvirtchild eincvirtchild esetcontext

" SMX instructions
syn keyword asmSmxIns getsec capabilities enteraccs exitac parameters senter sexit smctrl
syn keyword asmSmxIns wakeup

" VMX instructions
syn keyword asmVmxIns invept invvpid vmclear vmcall vmfunc vmlaunch vmptrld vmptrst vmread
syn keyword asmVmxIns vmresume vmwrite vmxoff vmxon

" Xeon Phi instructions
syn keyword asmXphIns prefetchwt1 v4fmaddps v4fnmaddps v4fmaddss v4fnmaddss vexp2pd vexp2ps
syn keyword asmXphIns vgatherpf0dps vgatherpf0qps vgatherpf0dpd vgatherpf0qpd vgatherpf1dps
syn keyword asmXphIns vgatherpf1qps vgatherpf1dpd vgatherpf1qpd vp4dpwssd vp4dpwssds
syn keyword asmXphIns vrcp28pd vrcp28ps vrcp28sd vrcp28ss vrsqrt28ps vrsqrt28pd vrsqrt28sd
syn keyword asmXphIns vrsqrt28ss vscatterpf0dpd vscatterpf0dps vscatterpf0qpd vscatterpf0qps
syn keyword asmXphIns vscatterpf1dpd vscatterpf1dps vscatterpf1qpd vscatterpf1qps




""""""""""""""""""""""""""""""
""" SYNTAX SYNCHRONIZATION """
""""""""""""""""""""""""""""""


syn sync clear
syn sync minlines=15



""""""""""""""""""""""""""
""" LINK SYNTAX GROUPS """
""""""""""""""""""""""""""

" COMMENTS

hi link asmComment           Comment
hi link asmTodo              Todo

" DIRECTIVES

hi link asmLabel             Label
hi link asmIdentifier        asmLabel
hi link asmType              Type
hi link asmPreProc           PreProc
hi link asmMacro             Macro
hi link asmInclude           Include
hi link asmDbg               Debug
hi link asmStdDirective      PreProc

" CONSTANTS

hi link asmString            String
hi link asmSpecial           Special
hi link asmCharacter         Constant
hi link asmBinNumber         Number
hi link asmOctNumber         Number
hi link	asmDecNumber         Number
hi link asmHexNumber         Number
hi link asmFltNumber         Number

" REGISTERS

hi link asmPcRegister        asmGenRegister
hi link asmSegRegister       asmGenRegister
hi link asmFpuRegister       asmGenRegister
hi link asmMmxRegister       asmVecRegister
hi link asmVecRegister       asmGenRegister
hi link asmCtrlRegister      asmGenRegister
hi link asmDbgRegister       asmGenRegister
hi link asmGenRegister       Identifier

" DATA SIZES

hi link asmDataSize          Type

" INSTRUCTIONS

hi link asmSysIns            asmStdIns
hi link asmExtIns            asmStdIns
hi link asmFpuIns            asmStdIns
hi link asmMmxIns            asmAvxIns
hi link asmSseIns            asmAvxIns
hi link asmAvxIns            asmStdIns
hi link asmSgxIns            asmStdIns
hi link asmSmxIns            asmStdIns
hi link asmVmxIns            asmStdIns
hi link asmXphIns            asmAvxIns
hi link asmStdIns            Operator




let b:current_syntax = "asmIntel"
