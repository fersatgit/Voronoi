MAX_THREADS=64
MAX_POINTS =999

format PE64 GUI 4.0 DLL as 'cpg'
entry DllEntryPoint
include 'encoding\win1251.inc'
include 'win64w.inc'

prologue@proc equ static_rsp_prologue
epilogue@proc equ static_rsp_epilogue
close@proc equ static_rsp_close

align 16
data import
  library kernel,'KERNEL32.DLL',\
          oleaut,'OLEAUT32.DLL',\
          user,'USER32'

  import kernel,\
         VirtualAlloc,'VirtualAlloc',\
         VirtualFree,'VirtualFree',\
         CreateThread,'CreateThread',\
         WaitForMultipleObjects,'WaitForMultipleObjects',\
         GetSystemInfo,'GetSystemInfo'

  import oleaut,\
         SafeArrayDestroy,'SafeArrayDestroy'

  import user,\
         MessageBoxW,'MessageBoxW'
end data

data export
    export 0,AttachPlugin,'AttachPlugin'
end data

data fixups
end data

include 'CorelDraw.inc'

DllEntryPoint: ;hinstDLL,fdwReason,lpvReserved
  mov eax,TRUE
ret

AttachPlugin: ;ppIPlugin: IVGAppPlugin
  mov qword[rcx],IPlugin
  mov eax,256
ret

;loword(lParam) - starting point
;hiword(lParam) - number of locus to generate
VoronoiThread: ;(lParam: dword);
  .Locus equ rsp-MAX_POINTS*16-8 ;local array for locus points aligned to 16 bytes boundary
  movzx  ebp,cx ;loword(lParam)
  shr    ecx,16
  shl    ebp,4
  movzx  r8,cx
  movdqu xmm7,dqword[dbl_exp_1]
  .P1:sub    ebp,16
      lea    rdi,[.Locus]
      mov    rbx,[Points]
      mov    ecx,[PointsLen]

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Searching Voronoi diagramm locus points
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      jmp .P2Start
      .P2:cmp    ecx,ebp
          je .P2Start
            mov    edx,ecx
            jmp .P3Start
            .P3:cmp    edx,ebp
                je .P3Start
                  movapd xmm0,[rbx+rbp]  ;P1
                  movapd xmm1,[rbx+rcx]  ;P2
                  movapd xmm2,[rbx+rdx]  ;P3
                  movapd xmm3,xmm1
                  movapd xmm4,xmm2
                  subpd  xmm3,xmm0
                  subpd  xmm4,xmm1
                  movapd xmm5,xmm3
                  movapd xmm6,xmm4
                  shufpd xmm3,xmm3,1
                  shufpd xmm4,xmm4,1
                  divpd  xmm3,xmm5
                  divpd  xmm4,xmm6
                  movapd xmm5,xmm0
                  addpd  xmm0,xmm1
                  subpd  xmm5,xmm2
                  addpd  xmm1,xmm2
                  shufpd xmm5,xmm5,1
                  mulpd  xmm0,xmm4
                  mulpd  xmm5,xmm3
                  mulpd  xmm1,xmm3
                  mulpd  xmm5,xmm4
                  subpd  xmm4,xmm3
                  addpd  xmm0,xmm5
                  subpd  xmm0,xmm1
                  divpd  xmm0,xmm4
                  psubq  xmm0,xmm7
                  movupd xmm1,xmm0       ;xmm1 - center of the cyrcle by 3 points (P1,P2,P3)
                  subpd  xmm0,xmm2
                  dppd   xmm0,xmm0,$33
                  addsd  xmm0,[dbl_1]    ;xmm0 - circle radius incrementing by 1 to avoid ZF-flag setting in the next step (ZF will be indicate to floating point overflow)

                  mov    eax,[PointsLen]
                  mov    esi,4
                  .DelonayCheck:         ;counting points inside this circle
                    movapd xmm2,[rbx+rax-16]
                    subpd  xmm2,xmm1
                    dppd   xmm2,xmm2,$33
                    comisd xmm2,xmm0
                    ja @f                ;if point inside radius or radius is infinite
                       dec esi
                       je .P3Start       ;if circle contains more than 3 points - skip it
                    @@:
                    sub    eax,16
                  jne .DelonayCheck
                  movupd [rdi],xmm1
                  add    rdi,16
                .P3Start:
                sub edx,16
            jns .P3
          .P2Start:
          sub ecx,16
      jns .P2
      lea rax,[.Locus]
      sub rdi,rax

      cmp edi,48
      jb .P1Start
          mov    ebx,edi
          shr    ebx,4
          inc    ebx
          lock   xadd [CurveElementsLen],ebx
          shl    ebx,5
          add    rbx,[CurveElements]
          mov    rsi,rbx

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Making convex hull for the locus
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          movapd xmm6,[.Locus+rdi-16]
          movapd dqword[rbx+CurveElement.PositionX],xmm6
          mov    [rbx+CurveElement.ElementType],cdrElementStart
          mov    [rbx+CurveElement.NodeType],cdrCuspNode
          mov    [rbx+CurveElement.Flags],cdrFlagValid+cdrFlagUser+cdrFlagClosed
          add    rbx,32
          sub    edi,32
          .ConvexHull:
              mov edx,edi
              .NxtVertex:
                movupd xmm3,dqword[chs]
                movapd xmm2,[rbx-32]
                movapd xmm1,[.Locus+rdx]
                movapd xmm5,xmm1
                xorpd  xmm4,xmm4
                subpd  xmm1,xmm2
                shufpd xmm1,xmm1,1
                xorpd  xmm1,xmm3
                mov    eax,edi
                movapd xmm6,[.Locus+rax]
                .CheckConvex:
                  cmp eax,edx
                  je @f
                    movapd xmm0,[.Locus+rax]
                    subpd  xmm0,xmm2
                    dppd   xmm0,xmm1,$33
                    comisd xmm0,xmm4
                    ja .nxt
                  @@:
                  sub eax,16
                jns .CheckConvex
                movapd dqword[rbx+CurveElement.PositionX],xmm5
                mov    [rbx+CurveElement.ElementType],cdrElementLine
                mov    [rbx+CurveElement.NodeType],cdrCuspNode
                mov    [rbx+CurveElement.Flags],cdrFlagValid+cdrFlagUser
                movapd [.Locus+rdx],xmm6
                sub    edi,16
                add    rbx,32
                .nxt:
                sub edx,16
              jns .NxtVertex
              test edi,edi
          jns .ConvexHull
          movapd dqword[rbx+CurveElement.PositionX],xmm6
          mov    [rbx+CurveElement.ElementType],cdrElementLine
          mov    [rbx+CurveElement.NodeType],cdrCuspNode
          mov    [rbx+CurveElement.Flags],cdrFlagValid+cdrFlagUser+cdrFlagClosed
          add    rbx,32

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Shrinking polygon to avoid CorelDraw glitches upon intersection
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          lea    rax,[rbx-64]
          .Shrink:
            movapd   xmm1,[rax-32]
            movapd   xmm0,[rax]
            subpd    xmm1,[rax+32]
            movapd   xmm2,xmm1
            dppd     xmm1,xmm1,$33
            shufpd   xmm2,xmm2,1
            sqrtsd   xmm1,xmm1
            mulpd    xmm2,dqword[dbl_0001] ;offsetting vertex by 1 micron
            shufpd   xmm1,xmm1,0
            divpd    xmm2,xmm1
            addsubpd xmm0,xmm2
            movntpd  [rax],xmm0
            sub      rax,32
            cmp      rax,rsi
          ja .Shrink
          movapd   xmm1,[rbx-64]
          movapd   xmm0,[rsi]
          subpd    xmm1,[rsi+32]
          movapd   xmm2,xmm1
          dppd     xmm1,xmm1,$33
          shufpd   xmm2,xmm2,1
          sqrtsd   xmm1,xmm1
          mulpd    xmm2,dqword[dbl_0001]
          shufpd   xmm1,xmm1,0
          divpd    xmm2,xmm1
          addsubpd xmm0,xmm2
          movntpd  [rsi],xmm0
          movntpd  [rbx-32],xmm0
      .P1Start:
      dec r8  ;hiword(lParam)
  jne .P1
ret

;;;;;;;;;;;;;;;ITypeComp;;;;;;;;;;;;;;;;;;;;;;
proc Bind this,szName,lHashVal,wflags,tinfo,desckind,bindptr
  movzx rax,byte[rdx]
  sub   eax,'0'
  mov   [funcdesc.memid],rax
  mov   rax,[desckind]
  mov   dword[rax],DESCKIND_FUNCDESC
  mov   rax,[bindptr]
  mov   qword[rax],funcdesc
  xor   eax,eax
  mov   [funcdesc.invkind],r9d
  cmp   r9w,INVOKE_PROPERTYGET
  jne @f
    mov [funcdesc.cParams],0
    mov [funcdesc.elemdescFunc.tdesc.vt],VT_R8
    ret
  @@:
  cmp   r9w,INVOKE_PROPERTYPUT
  jne @f
    mov [funcdesc.cParams],1
    mov [funcdesc.elemdescFunc.tdesc.vt],VT_VOID
    ret
  @@:
  cmp   r9w,INVOKE_FUNC
  jne @f
    mov [funcdesc.cParams],0
    mov [funcdesc.elemdescFunc.tdesc.vt],VT_VOID
    ret
  @@:
  ret
endp

;;;;;;;;;;;;;;;ITypeInfo;;;;;;;;;;;;;;;;;;;;;;
GetTypeComp:          ;(self: ITypeInfo; out tcomp: ITypeComp): HResult; stdcall;
  mov qword[rdx],ITypeComp
  xor eax,eax
ret

;;;;;;;;;;;;;;;IVGPlugin;;;;;;;;;;;;;;;;;;;;;;
QueryInterface:   ;(const self:IVGAppPlugin; const IID: TGUID; out Obj): HResult; stdcall;
  mov qword[r8],IPlugin
StopSession:      ;(const self:IVGAppPlugin):LongInt;stdcall;
AddRef:           ;(const self:IVGAppPlugin):Integer; stdcall;
Release:          ;(const self:IVGAppPlugin):Integer; stdcall;
  xor eax,eax
ret
GetTypeInfoCount: ;(const self:IVGAppPlugin; out Count: Integer): HResult; stdcall;
  mov dword[rdx],1
  xor eax,eax
ret
GetTypeInfo:      ;(const self:IVGAppPlugin; Index, LocaleID: Integer; out TypeInfo): HResult; stdcall;
  mov qword[r9],ITypeInfo
  xor eax,eax
ret
NotImplStub:
  mov eax,E_NOTIMPL
ret

proc Invoke this,DispID,IID,LocaleID,Flags,Params,VarResult,ExcepInfo,ArgErr
  test rdx,rdx
  jne @f
    cominvk FrameWork,ShowDialog,strVoronoi
    ret
  @@:
  cmp rdx,4
  jae .OnInvoke
    test word[Flags],INVOKE_PROPERTYGET
    je @f
      movsd xmm0,[NumPoints+rdx*8-8]
      mov   rax,[VarResult]
      mov   dword[rax+VARIANT.type],VT_R8
      movsd [rax+VARIANT.data],xmm0
      xor   eax,eax
      ret
    @@:
    test word[Flags],INVOKE_PROPERTYPUT
    je @f
      mov   rax,[Params]
      mov   rax,[rax+DISPPARAMS.rgvarg]
      movsd xmm0,[rax+VARIANT.data]
      movsd [NumPoints+rdx*8-8],xmm0
    @@:
    xor eax,eax
    ret
  .OnInvoke:
  ja .finish
    sub    rsp,32
    movdqa [rsp],xmm6
    movdqa [rsp+16],xmm7
    push   rbx
    push   rbp
    push   rsi
    push   rdi
    frame
    rdtsc
    mul      rax
    xor      [rndseed],rax
    xor      [rndseed+8],rax
    mov      rbx,[CorelApp]
    comcall  rbx,IVGApplication,Get_ActiveDocument,CorelDoc
    comcall  rbx,IVGApplication,Get_ActiveSelectionRange,Selection
    comcall  rbx,IVGApplication,CreateCurve,[CorelDoc],Curve
    mov      rbx,[CorelDoc]
    comcall  rbx,IVGDocument,BeginCommandGroup,0
    comcall  rbx,IVGDocument,Set_Unit,cdrMillimeter
    comcall  rbx,IVGDocument,Get_ActiveLayer,Layer
    mov      rbx,[Selection]
    comcall  rbx,IVGShapeRange,Combine,Shape
    comcall  rbx,IVGShapeRange,Release
    mov      rbx,[Shape]
    comcall  rbx,IVGShape,GetSize,Width,Height
    comcall  rbx,IVGShape,GetPositionEx,cdrBottomLeft,x,y

    movsd    xmm0,[NumPoints]
    cvtsd2si ebx,xmm0
    mov      eax,ebx
    mul      eax
    lea      rdx,[eax+ebx]
    shl      edx,5
    shl      ebx,4
    mov      [PointsLen],ebx
    invoke   VirtualAlloc,0,rdx,MEM_COMMIT,PAGE_READWRITE
    mov      [Points],rax

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Random points generation
;RNG Algo from George Marsaglia "Xorshift RNGs" - https://www.researchgate.net/publication/5142825_Xorshift_RNGs
;Floating point conversion Algo from JURGEN A DOORNIK "Conversion of High-Period Random Numbers to Floating Point" - www.doornik.com/research/randomdouble.pdf
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    movapd   xmm0,dqword[x]
    movapd   xmm1,dqword[Width]
    addpd    xmm1,xmm0
    subpd    xmm0,dqword[dbl_1]
    addpd    xmm1,dqword[dbl_1]
    movapd   xmm2,xmm0
    movapd   xmm3,xmm1
    shufpd   xmm2,xmm1,10b
    shufpd   xmm3,xmm0,10b
    movapd   [rax   ],xmm0
    movapd   [rax+16],xmm1
    movapd   [rax+32],xmm2
    movapd   [rax+48],xmm3
    lea      ecx,[ebx-64]
    movdqu   xmm1,dqword[rndseed]
    @@:movdqa   xmm3,xmm1
       psllq    xmm1,13
       pxor     xmm1,xmm3
       movdqa   xmm3,xmm1
       psrlq    xmm1,7
       pxor     xmm1,xmm3
       movdqa   xmm3,xmm1
       psllq    xmm1,17
       pxor     xmm1,xmm3
       movdqa   xmm3,xmm1
       movdqa   xmm0,xmm1
       psllq    xmm3,12
       psrad    xmm3,12
       shufps   xmm0,xmm0,1000b
       shufps   xmm3,xmm3,1101b
       cvtdq2pd xmm0,xmm0
       cvtdq2pd xmm3,xmm3
       mulpd    xmm0,dqword[rndMagic2]
       mulpd    xmm3,dqword[rndMagic1]
       addpd    xmm0,dqword[dbl_05]
       addpd    xmm0,xmm3
       mulpd    xmm0,dqword[Width]
       addpd    xmm0,dqword[x]
       movntpd  [rax+64+rcx-16],xmm0
       sub      ecx,16
    jne @b
    mov      [CurveElementsLen],ecx
    mov      [Selection],rcx

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Main calculation routine (multithreaded)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    lea      rsi,[rax+rbx]
    mov      [CurveElements],rsi
    invoke   GetSystemInfo,sysInfo
    mov      ebp,ebx
    mov      edx,1
    mov      ecx,[sysInfo.dwNumberOfProcessors]
    shr      ebp,8
    cmove    ebp,edx
    mov      edx,MAX_THREADS
    cmp      ebp,ecx
    cmova    ebp,ecx
    cmp      ebp,edx
    cmova    ebp,edx
    shr      ebx,4
    mov      eax,ebx
    cdq
    div      ebp
    mov      ebp,eax
    mov      rdi,Threads
    @@:cmp    bx,bp
       cmovb  bp,bx
       shl    ebx,16
       shrd   ebx,ebp,16
       invoke CreateThread,0,MAX_POINTS*16+1024,VoronoiThread,ebx,0,0
       stosq
       sub    bx,bp
    jne @b
    sub      rdi,Threads
    shr      edi,3
    invoke   WaitForMultipleObjects,edi,Threads,1,-1

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Postprocessing (Intersection with shape, Contour and Fillet)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    mov      rbx,[Curve]
    comcall  rbx,IVGCurve,GetCurveInfo,CurveInfo
    mov      rbp,[CurveInfo]
    mov      eax,[CurveElementsLen]
    movdqu   xmm7,dqword[rbp+SAFEARRAY.pvData]
    mov      [rbp+SAFEARRAY.pvData],rsi
    xor      esi,esi
    mov      [rbp+SAFEARRAY.rgsabound.cElements],eax
    comcall  rbx,IVGCurve,PutCurveInfo,CurveInfo,eax,Shape2
    cominvk  Layer,CreateCurve,[Curve],Shape2
    comcall  rbx,IVGCurve,Release
    cominvk  Shape2,Intersect,[Shape],0,0,Shape3
    cominvk  Shape,Release
    cominvk  Shape2,Release
    xorpd    xmm0,xmm0
    comisd   xmm0,[Gap]
    je @f
      cominvk CorelApp,CreateCMYKColor,100,100,100,100,Color
      mov     rbx,[Shape3]
      mov     rax,[Color]
      comcall rbx,IVGShape,CreateContour,cdrContourInside,float[Gap],1,cdrDirectFountainFillBlend,rax,rax,rax,rsi,rsi,cdrContourSquareCap,cdrContourCornerMiteredOffsetBevel,15.0,Contour
      comcall rbx,IVGShape,Release
      cominvk Color,Release
      mov     rbx,[Contour]
      comcall rbx,IVGEffect,Separate,Selection
      comcall rbx,IVGEffect,Release
      mov     rbx,[Selection]
      test    rbx,rbx
      je .quit
        mov     [var.data],1
        comcall rbx,IVGShapeRange,Get_Item,var,Shape3
        mov     [var.data],2
        comcall rbx,IVGShapeRange,Get_Item,var,Shape
        comcall rbx,IVGShapeRange,Release
        mov     rbx,[Shape]
        comcall rbx,IVGShape,Delete
        comcall rbx,IVGShape,Release
    @@:
    xorpd   xmm0,xmm0
    comisd  xmm0,[Fillet]
    je @f
      cominvk Shape3,Fillet,float[Fillet],1
    @@:

    cominvk  Shape3,Release
    .quit:
    cominvk  Layer,Release
    cominvk  CorelDoc,EndCommandGroup
    cominvk  CorelApp,Refresh
    movdqu   dqword[rbp+SAFEARRAY.pvData],xmm7
    invoke   SafeArrayDestroy,rbp
    invoke   VirtualFree,[Points],rsi,MEM_RELEASE
    cominvk  FrameWork,HideDialog,strVoronoi
    endf
    pop      rdi
    pop      rsi
    pop      rbp
    pop      rbx
    movdqa   xmm6,[rsp]
    movdqa   xmm7,[rsp+16]
    add      rsp,32
  .finish:
  xor eax,eax
  ret
endp

proc OnLoad this
  mov     [CorelApp],rbx
  mov     rbx,rdx
  comcall rbx,IVGApplication,AddRef
  comcall rbx,IVGApplication,Get_FrameWork,FrameWork
  comcall rbx,IVGApplication,QueryInterface,IID_ICUIApplication,CUIApp
  cominvk CUIApp,RegisterDataSource,strVoronoi,DataSourceFactory,NullStr,0,NullStr  ;I`ve tried CUIApp.DataContext.AddDataSource, but it cause exception at the CorelDraw closing (look`s like Corel try to release interface after dll is unloaded)
  xchg    [CorelApp],rbx
  ret
endp

proc StartSession this
  mov   eax,1
  cpuid
  xor   eax,eax
  and   ecx,1 shl 19 ;SSE 4.1
  jne @f
    invoke  MessageBoxW,0,errCPUNotSupported,strVoronoi,MB_TASKMODAL
    stdcall OnUnload,rdx
    mov     eax,E_FAIL
  @@:
  ret
endp

proc OnUnload this
  cominvk CUIApp,UnregisterDataSource,strVoronoi,NullStr
  cominvk CUIApp,Release
  cominvk CorelApp,Release
  xor     eax,eax
  ret
endp

;;;;;;;;;;;;;;;ICUIDataSourceFactory;;;;;;;;;;;;;;;;;;;;;;
CreateDataSource: ;((const self:ICUIDataSourceFactory;const DataSourceName: WideString; const Proxy: ICUIDataSourceProxy; out ppVal: IDispatch); safecall;
  mov qword[r9],IPlugin
  xor eax,eax
ret

align 16
dbl_1                dq 1.0,1.0
dbl_05               dq 0.5,0.5
dbl_0001             dq 0.001,0.001
chs                  dq $8000000000000000,0
dbl_exp_1            dq 1 shl 52,1 shl 52
rndseed              dq 88172645463325252,154793495671876040
rndMagic1            dq 2.22044604925031308085e-016,2.22044604925031308085e-016
rndMagic2            dq 2.32830643653869628906e-010,2.32830643653869628906e-010
NumPoints            dq 50.0
Gap                  rq 1
Fillet               rq 1
var                  VARIANT VT_I4
IPlugin              dq IPluginVMT
IPluginVMT           dq QueryInterface,\
                        AddRef,\
                        Release,\
                        GetTypeInfoCount,\
                        GetTypeInfo,\
                        NotImplStub,\;GetIDsOfNames,\
                        Invoke,\
                        OnLoad,\
                        StartSession,\
                        StopSession,\
                        OnUnload
DataSourceFactory    dq DataSourceFactoryVMT
DataSourceFactoryVMT dq QueryInterface,\
                        AddRef,\
                        Release,\
                        NotImplStub,\;GetTypeInfoCount,\
                        NotImplStub,\;GetTypeInfo,\
                        NotImplStub,\;GetIDsOfNames,\
                        NotImplStub,\;Invoke,\
                        CreateDataSource
ITypeInfo            dq ITypeInfoVMT
ITypeInfoVMT         dq QueryInterface,\
                        AddRef,\
                        Release,\
                        NotImplStub,\;GetTypeAttr,\
                        GetTypeComp,\
                        NotImplStub,\;GetFuncDesc,\
                        NotImplStub,\;GetVarDesc,\
                        NotImplStub,\;GetNames,\
                        NotImplStub,\;GetRefTypeOfImplType,\
                        NotImplStub,\;GetImplTypeFlags,\
                        NotImplStub,\;ITypeInfo.GetIDsOfNames,\
                        NotImplStub,\;ITypeInfo.Invoke,\
                        NotImplStub,\;GetDocumentation,\
                        NotImplStub,\;GetDllEntry,\
                        NotImplStub,\;GetRefTypeInfo,\
                        NotImplStub,\;AddressOfMember,\
                        NotImplStub,\;CreateInstance,\
                        NotImplStub,\;GetMops,\
                        NotImplStub,\;GetContainingTypeLib,\
                        NotImplStub,\;ReleaseTypeAttr,\
                        NotImplStub,\;ReleaseFuncDesc,\
                        NotImplStub;ReleaseVarDesc
ITypeComp            dq ITypeCompVMT
ITypeCompVMT         dq QueryInterface,\
                        AddRef,\
                        Release,\
                        Bind,\
                        NotImplStub;BindType

strVoronoi           OLEstr 'Voronoi'
errCPUNotSupported   du 'Процессор не поддерживается. Требуется SSE 4.1.',0
params               ELEMDESC 0,VT_R8
funcdesc             FUNCDESC 0,0,params,FUNC_DISPATCH,0,CC_STDCALL

align 16
x                 rq 1
y                 rq 1
Width             rq 1
Height            rq 1
CorelApp          IVGApplication
CUIApp            ICUIApplication
FrameWork         ICUIFrameWork
CorelDoc          IVGDocument
Layer             IVGLayer
Shape             IVGShape
Shape2            IVGShape
Shape3            IVGShape
Curve             IVGCurve
Selection         IVGShapeRange
Color             IVGColor
Contour           IVGEffect
CurveInfo         rq 1
Points            rq 1
CurveElements     rq 1
CurveElementsLen  rd 1
PointsLen         rd 1
Threads           rq MAX_THREADS
sysInfo           SYSTEM_INFO
NullStr           rq 1