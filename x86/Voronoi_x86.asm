MAX_THREADS=64
MAX_POINTS =999

format PE GUI 4.0 DLL as 'cpg'
entry DllEntryPoint
include 'encoding\win1251.inc'
include 'win32w.inc'

section '' readable writeable executable

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
ret 12

AttachPlugin: ;ppIPlugin: IVGAppPlugin
  mov eax,[esp+4]
  mov dword[eax],IPlugin
  mov eax,256
ret 4

;loword(lParam) - starting point
;hiword(lParam) - number of locus to generate
VoronoiThread: ;(lParam: dword);
.Locus equ esp-MAX_POINTS*16 ;local array for locus points
  movzx  ebp,word[esp+4] ;loword(lParam)
  shl    ebp,4
  movdqu xmm7,dqword[dbl_exp_1]
  .P1:sub    ebp,16
      lea    edi,[.Locus]
      mov    ebx,[Points]
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
                  movapd xmm0,[ebx+ebp]  ;P1
                  movapd xmm1,[ebx+ecx]  ;P2
                  movapd xmm2,[ebx+edx]  ;P3
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
                  .DelonayCheck:
                    movapd xmm2,[ebx+eax-16]
                    subpd  xmm2,xmm1
                    dppd   xmm2,xmm2,$33
                    comisd xmm2,xmm0
                    ja @f                ;if point inside radius or radius is infinite
                       dec esi
                       je .P3Start       ;if circle contains more than 3 points - skip it
                    @@:
                    sub    eax,16
                  jne .DelonayCheck
                  movupd [edi],xmm1
                  add    edi,16
                .P3Start:
                sub edx,16
            jns .P3
          .P2Start:
          sub ecx,16
      jns .P2
      lea eax,[.Locus]
      sub edi,eax

      cmp edi,48
      jb .P1Start
          mov    ebx,edi
          shr    ebx,4
          inc    ebx
          lock   xadd [CurveElementsLen],ebx
          shl    ebx,5
          add    ebx,[CurveElements]
          mov    esi,ebx

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Making convex hull for the locus
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          movupd xmm0,[.Locus+edi-16]
          movapd dqword[ebx+CurveElement.PositionX],xmm0
          mov    [ebx+CurveElement.ElementType],cdrElementStart
          mov    [ebx+CurveElement.NodeType],cdrCuspNode
          mov    [ebx+CurveElement.Flags],cdrFlagValid+cdrFlagUser+cdrFlagClosed
          add    ebx,32
          sub    edi,32
          .ConvexHull:
              mov edx,edi
              .NxtVertex:
                movupd xmm3,dqword[chs]
                movapd xmm2,[ebx-32]
                movupd xmm1,[.Locus+edx]
                movapd xmm5,xmm1
                xorpd  xmm4,xmm4
                subpd  xmm1,xmm2
                shufpd xmm1,xmm1,1
                xorpd  xmm1,xmm3
                mov    eax,edi
                .CheckConvex:
                  cmp eax,edx
                  je @f
                    movupd xmm0,[.Locus+eax]
                    subpd  xmm0,xmm2
                    dppd   xmm0,xmm1,$33
                    comisd xmm0,xmm4
                    ja .nxt
                  @@:
                  sub eax,16
                jns .CheckConvex
                movupd xmm0,[.Locus+edi]
                movapd dqword[ebx+CurveElement.PositionX],xmm5
                mov    [ebx+CurveElement.ElementType],cdrElementLine
                mov    [ebx+CurveElement.NodeType],cdrCuspNode
                mov    [ebx+CurveElement.Flags],cdrFlagValid+cdrFlagUser
                movupd [.Locus+edx],xmm0
                sub    edi,16
                add    ebx,32
                .nxt:
                sub edx,16
              jns .NxtVertex
              test edi,edi
          jns .ConvexHull
          ;movapd xmm0,dqword[esi+CurveElement.PositionX]
          ;movapd dqword[ebx+CurveElement.PositionX],xmm0  ;It will be done after shrinking
          mov    [ebx+CurveElement.ElementType],cdrElementLine
          mov    [ebx+CurveElement.NodeType],cdrCuspNode
          mov    [ebx+CurveElement.Flags],cdrFlagValid+cdrFlagUser+cdrFlagClosed
          add    ebx,32

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Shrinking polygon to avoid CorelDraw glitches upon intersection
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          lea    eax,[ebx-64]
          .Shrink:
            movapd   xmm1,[eax-32]
            movapd   xmm0,[eax]
            subpd    xmm1,[eax+32]
            movapd   xmm2,xmm1
            dppd     xmm1,xmm1,$33
            shufpd   xmm2,xmm2,1
            sqrtsd   xmm1,xmm1
            mulpd    xmm2,dqword[dbl_0001] ;offsetting vertex by 1 micron
            shufpd   xmm1,xmm1,0
            divpd    xmm2,xmm1
            addsubpd xmm0,xmm2
            movntpd  [eax],xmm0
            sub      eax,32
            cmp      eax,esi
          ja .Shrink
          movapd   xmm1,[ebx-64]
          movapd   xmm0,[esi]
          subpd    xmm1,[esi+32]
          movapd   xmm2,xmm1
          dppd     xmm1,xmm1,$33
          shufpd   xmm2,xmm2,1
          sqrtsd   xmm1,xmm1
          mulpd    xmm2,dqword[dbl_0001]
          shufpd   xmm1,xmm1,0
          divpd    xmm2,xmm1
          addsubpd xmm0,xmm2
          movntpd  [esi],xmm0
          movntpd  [ebx-32],xmm0
      .P1Start:
      dec word[esp+6]  ;hiword(lParam)
  jne .P1
ret 4

;;;;;;;;;;;;;;;ITypeComp;;;;;;;;;;;;;;;;;;;;;;
Bind: ;(self: ITypeComp; const szName: WideString; lHashVal: Longint; wflags: Word; out tinfo: ITypeInfo; out desckind: Longint; out bindptr: pointer): HResult; stdcall;
  mov   eax,[esp+8]
  movzx eax,byte[eax]
  sub   eax,'0'
  mov   [funcdesc.memid],eax
  mov   eax,[esp+24]
  mov   dword[eax],DESCKIND_FUNCDESC
  mov   eax,[esp+28]
  mov   dword[eax],funcdesc
  movzx edx,word[esp+16]
  xor   eax,eax
  mov   [funcdesc.invkind],edx
  cmp   edx,INVOKE_PROPERTYGET
  jne @f
    mov [funcdesc.cParams],0
    mov [funcdesc.elemdescFunc.tdesc.vt],VT_R8
    ret 28
  @@:
  cmp   edx,INVOKE_PROPERTYPUT
  jne @f
    mov [funcdesc.cParams],1
    mov [funcdesc.elemdescFunc.tdesc.vt],VT_VOID
    ret 28
  @@:
  cmp   edx,INVOKE_FUNC
  jne @f
    mov [funcdesc.cParams],0
    mov [funcdesc.elemdescFunc.tdesc.vt],VT_VOID
    ret 28
  @@:
ret 28

;;;;;;;;;;;;;;;ITypeInfo;;;;;;;;;;;;;;;;;;;;;;
ReleaseTypeAttr:      ;(self: ITypeInfo; ptypeattr: PTypeAttr); stdcall;
ReleaseFuncDesc:      ;(self: ITypeInfo; pfuncdesc: pointer); stdcall;
ReleaseVarDesc:       ;(self: ITypeInfo; pvardesc: PVarDesc); stdcall;
GetTypeAttr:          ;(self: ITypeInfo; out ptypeattr: PTypeAttr): HResult; stdcall;
  mov eax,E_NOTIMPL
ret 8

GetTypeComp:          ;(self: ITypeInfo; out tcomp: ITypeComp): HResult; stdcall;
  mov eax,[esp+8]
  mov dword[eax],ITypeComp
  xor eax,eax
ret 8

GetFuncDesc:             ;(self: ITypeInfo; index: Integer; out pfuncdesc: pointer): HResult; stdcall;
GetVarDesc:              ;(self: ITypeInfo; index: Integer; out pvardesc: PVarDesc): HResult; stdcall;
GetRefTypeOfImplType:    ;(self: ITypeInfo; index: Integer; out reftype: dword): HResult; stdcall;
GetImplTypeFlags:        ;(self: ITypeInfo; index: Integer; out impltypeflags: Integer): HResult; stdcall;
GetRefTypeInfo:          ;(self: ITypeInfo; reftype: dword; out tinfo: IUnknown): HResult;stdcall;
GetMops:                 ;(self: ITypeInfo; memid: Longint; out bstrMops: WideString): HResult;stdcall;
GetContainingTypeLib:    ;(self: ITypeInfo; out tlib: IUnknown; out pindex: Integer): HResult;stdcall;
  mov eax,E_NOTIMPL
ret 12

GetNames:                ;(self: ITypeInfo; memid: Longint; rgbstrNames: pointer; cMaxNames: Integer; out cNames: Integer): HResult; stdcall;
BindType:                ;(self: ITypeComp; const szName: WideString; lHashVal: Longint; out tinfo: ITypeInfo; out tcomp: ITypeComp): HResult; stdcall;
  mov eax,E_NOTIMPL
ret 20

ITypeInfo.GetIDsOfNames: ;(self: ITypeInfo; rgpszNames: pointer; cNames: Integer; rgmemid: pointer): HResult; stdcall;
AddressOfMember:         ;(self: ITypeInfo; memid: Longint; invkind: Longint; out ppv: Pointer): HResult; stdcall;
CreateInstance:          ;(self: ITypeInfo; const unkOuter: IUnknown; const iid: TGUID; out vObj): HResult; stdcall;
  mov eax,E_NOTIMPL
ret 16

ITypeInfo.Invoke:        ;(self: ITypeInfo; pvInstance: Pointer; memid: Longint; flags: Word; var dispParams: DispParams; varResult: PVariant; excepInfo: pointer; argErr: PInteger): HResult; stdcall;
  mov eax,E_NOTIMPL
ret 32

GetDocumentation:        ;(self: ITypeInfo; memid: Longint; pbstrName: PWideString; pbstrDocString: PWideString; pdwHelpContext: PLongint; pbstrHelpFile: PWideString): HResult; stdcall;
GetDllEntry:             ;(self: ITypeInfo; memid: Longint; invkind: Longint; bstrDllName, bstrName: PWideString; wOrdinal: PWord): HResult;stdcall;
  mov eax,E_NOTIMPL
ret 24


;;;;;;;;;;;;;;;IVGPlugin;;;;;;;;;;;;;;;;;;;;;;
QueryInterface:   ;(const self:IVGAppPlugin; const IID: TGUID; out Obj): HResult; stdcall;
  mov eax,[esp+12]
  mov dword[eax],IPlugin
  xor eax,eax
ret 12
StopSession:      ;(const self:IVGAppPlugin):Integer; stdcall;
AddRef:           ;(const self:IVGAppPlugin):Integer; stdcall;
Release:          ;(const self:IVGAppPlugin):Integer; stdcall;
  xor eax,eax
ret 4
GetTypeInfoCount: ;(const self:IVGAppPlugin; out Count: Integer): HResult; stdcall;
  mov eax,[esp+8]
  mov dword[eax],1
  xor eax,eax
ret 8
GetTypeInfo:      ;(const self:IVGAppPlugin; Index, LocaleID: Integer; out TypeInfo): HResult; stdcall;
  mov eax,[esp+16]
  mov dword[eax],ITypeInfo
  xor eax,eax
ret 16
GetIDsOfNames:    ;(const self:IVGAppPlugin; const IID: TGUID; Names: Pointer;NameCount, LocaleID: Integer; DispIDs: Pointer): HResult; stdcall;
  mov eax,E_NOTIMPL
ret 24


Invoke:           ;(const self:IVGAppPlugin; DispID: Integer; const IID: TGUID; LocaleID: Integer;Flags: Word; var Params; VarResult, ExcepInfo, ArgErr: Pointer): HResult; stdcall;
  mov  eax,[esp+8]
  test eax,eax
  jne @f
    cominvk FrameWork,ShowDialog,strVoronoi
    ret 36
  @@:
  cmp eax,4
  jae .OnInvoke
    test word[esp+20],INVOKE_PROPERTYGET ;Flags
    je @f
      movsd xmm0,[NumPoints+eax*8-8]
      mov   eax,[esp+28]
      mov   dword[eax+VARIANT.type],VT_R8
      movsd [eax+VARIANT.data],xmm0
      xor   eax,eax
      ret 36
    @@:
    test word[esp+20],INVOKE_PROPERTYPUT ;Flags
    je @f
      mov   edx,[esp+24]
      mov   edx,[edx+DISPPARAMS.rgvarg]
      movsd xmm0,[edx+VARIANT.data]
      movsd [NumPoints+eax*8-8],xmm0
    @@:
    xor eax,eax
    ret 36
  .OnInvoke:
  ja .finish
    pushad
    rdtsc
    mul      eax
    xor      dword[rndseed],eax
    xor      dword[rndseed+4],edx
    xor      dword[rndseed+8],eax
    xor      dword[rndseed+12],edx
    mov      ebx,[CorelApp]
    comcall  ebx,IVGApplication,Get_ActiveDocument,CorelDoc
    comcall  ebx,IVGApplication,Get_ActiveSelectionRange,Selection
    comcall  ebx,IVGApplication,CreateCurve,[CorelDoc],Curve
    mov      ebx,[CorelDoc]
    comcall  ebx,IVGDocument,BeginCommandGroup,0
    comcall  ebx,IVGDocument,Set_Unit,cdrMillimeter
    comcall  ebx,IVGDocument,Get_ActiveLayer,Layer
    mov      ebx,[Selection]
    comcall  ebx,IVGShapeRange,Combine,Shape
    comcall  ebx,IVGShapeRange,Release
    mov      ebx,[Shape]
    comcall  ebx,IVGShape,GetSize,Width,Height
    comcall  ebx,IVGShape,GetPositionEx,cdrBottomLeft,x,y

    movsd    xmm0,[NumPoints]
    cvtsd2si ebx,xmm0
    mov      eax,ebx
    mul      eax
    add      eax,ebx
    shl      eax,5
    shl      ebx,4
    mov      [PointsLen],ebx
    invoke   VirtualAlloc,0,eax,MEM_COMMIT,PAGE_READWRITE
    mov      [Points],eax

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
    movapd   [eax   ],xmm0
    movapd   [eax+16],xmm1
    movapd   [eax+32],xmm2
    movapd   [eax+48],xmm3
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
       movntpd  [eax+64+ecx-16],xmm0
       sub      ecx,16
    jne @b
    mov      [Selection],ecx
    mov      [CurveElementsLen],ecx

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Main calculation routine (multithreaded)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    lea      esi,[eax+ebx]
    mov      [CurveElements],esi
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
    mov      edi,Threads
    @@:cmp    bx,bp
       cmovb  bp,bx
       shl    ebx,16
       shrd   ebx,ebp,16
       invoke CreateThread,0,MAX_POINTS*16+1024,VoronoiThread,ebx,0,0
       stosd
       sub    bx,bp
    jne @b
    sub      edi,Threads
    shr      edi,2
    invoke   WaitForMultipleObjects,edi,Threads,1,-1

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Postprocessing (Intersection with shape, Contour and Fillet)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    mov      ebx,[Curve]
    comcall  ebx,IVGCurve,GetCurveInfo,CurveInfo
    mov      ebp,[CurveInfo]
    mov      eax,[CurveElementsLen]
    push     dword[ebp+SAFEARRAY.pvData]
    push     dword[ebp+SAFEARRAY.rgsabound.cElements]
    mov      [ebp+SAFEARRAY.pvData],esi
    xor      esi,esi
    mov      [ebp+SAFEARRAY.rgsabound.cElements],eax
    comcall  ebx,IVGCurve,PutCurveInfo,CurveInfo,eax,Shape2
    cominvk  Layer,CreateCurve,[Curve],Shape2
    comcall  ebx,IVGCurve,Release
    mov      ebx,[Shape2]
    comcall  ebx,IVGShape,Intersect,[Shape],esi,esi,Shape3
    comcall  ebx,IVGShape,Release
    cominvk  Shape,Release
    xorpd    xmm0,xmm0
    comisd   xmm0,[Gap]
    je @f
      cominvk CorelApp,CreateCMYKColor,100,100,100,100,Color
      mov     ebx,[Shape3]
      mov     eax,[Color]
      comcall ebx,IVGShape,CreateContour,cdrContourInside,dword[Gap],dword[Gap+4],1,cdrDirectFountainFillBlend,eax,eax,eax,esi,esi,cdrContourSquareCap,cdrContourCornerMiteredOffsetBevel,esi,$402E0000,Contour ;MiterLimit=15.0
      comcall ebx,IVGShape,Release
      cominvk Color,Release
      mov     ebx,[Contour]
      comcall ebx,IVGEffect,Separate,Selection
      comcall ebx,IVGEffect,Release
      mov     ebx,[Selection]
      test    ebx,ebx
      je      .quit
        comcall ebx,IVGShapeRange,Get_Item,VT_I4,esi,2,esi,Shape
        comcall ebx,IVGShapeRange,Get_Item,VT_I4,esi,1,esi,Shape3
        comcall ebx,IVGShapeRange,Release
        mov     ebx,[Shape]
        comcall ebx,IVGShape,Delete
        comcall ebx,IVGShape,Release
    @@:
    xorpd   xmm0,xmm0
    comisd  xmm0,[Fillet]
    je @f
      cominvk Shape3,Fillet,dword[Fillet],dword[Fillet+4],1
    @@:

    cominvk  Shape3,Release
    .quit:
    cominvk  Layer,Release
    cominvk  CorelDoc,EndCommandGroup
    cominvk  CorelApp,Refresh
    pop      dword[ebp+SAFEARRAY.rgsabound.cElements]
    pop      dword[ebp+SAFEARRAY.pvData]
    invoke   SafeArrayDestroy,ebp
    invoke   VirtualFree,[Points],esi,MEM_RELEASE
    cominvk  FrameWork,HideDialog,strVoronoi
    popad
  .finish:
  xor eax,eax
ret 36

OnLoad:           ;(const self:IVGAppPlugin; const _Application: IVGApplication):LongInt;stdcall;
  xchg    ebx,[esp+8]
  mov     [CorelApp],ebx
  comcall ebx,IVGApplication,AddRef
  comcall ebx,IVGApplication,Get_FrameWork,FrameWork
  comcall ebx,IVGApplication,QueryInterface,IID_ICUIApplication,CUIApp
  cominvk CUIApp,RegisterDataSource,strVoronoi,DataSourceFactory,NullStr,0,NullStr  ;I`ve tried CUIApp.DataContext.AddDataSource, but it cause exception at the CorelDraw closing (look`s like Corel try to release interface after dll is unloaded)
  mov     ebx,[esp+8]
ret 8

StartSession:     ;(const self:IVGAppPlugin):LongInt;stdcall;
  mov   eax,1
  cpuid
  xor   eax,eax
  and   ecx,1 shl 19 ;SSE 4.1
  jne @f
    invoke  MessageBoxW,0,errCPUNotSupported,strVoronoi,MB_TASKMODAL
    stdcall OnUnload,eax
    mov     eax,E_FAIL
  @@:
ret 4

OnUnload:         ;(const self:IVGAppPlugin)LongInt;stdcall;
  cominvk CUIApp,UnregisterDataSource,strVoronoi,NullStr
  cominvk CUIApp,Release
  cominvk CorelApp,Release
  xor     eax,eax
ret 4

;;;;;;;;;;;;;;;ICUIDataSourceFactory;;;;;;;;;;;;;;;;;;;;;;
CreateDataSource: ;((const self:ICUIDataSourceFactory;const DataSourceName: WideString; const Proxy: ICUIDataSourceProxy; out ppVal: IDispatch); safecall;
  mov edx,[esp+16]
  mov dword[edx],IPlugin
  xor eax,eax
ret 16

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
IPlugin              dd IPluginVMT
IPluginVMT           dd QueryInterface,\
                        AddRef,\
                        Release,\
                        GetTypeInfoCount,\
                        GetTypeInfo,\
                        GetIDsOfNames,\
                        Invoke,\
                        OnLoad,\
                        StartSession,\
                        StopSession,\
                        OnUnload
DataSourceFactory    dd DataSourceFactoryVMT
DataSourceFactoryVMT dd QueryInterface,\
                        AddRef,\
                        Release,\
                        GetTypeInfoCount,\
                        GetTypeInfo,\
                        GetIDsOfNames,\
                        Invoke,\
                        CreateDataSource
ITypeInfo            dd ITypeInfoVMT
ITypeInfoVMT         dd QueryInterface,\
                        AddRef,\
                        Release,\
                        GetTypeAttr,\
                        GetTypeComp,\
                        GetFuncDesc,\
                        GetVarDesc,\
                        GetNames,\
                        GetRefTypeOfImplType,\
                        GetImplTypeFlags,\
                        ITypeInfo.GetIDsOfNames,\
                        ITypeInfo.Invoke,\
                        GetDocumentation,\
                        GetDllEntry,\
                        GetRefTypeInfo,\
                        AddressOfMember,\
                        CreateInstance,\
                        GetMops,\
                        GetContainingTypeLib,\
                        ReleaseTypeAttr,\
                        ReleaseFuncDesc,\
                        ReleaseVarDesc
ITypeComp            dd ITypeCompVMT
ITypeCompVMT         dd QueryInterface,\
                        AddRef,\
                        Release,\
                        Bind,\
                        BindType

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
CurveInfo         rd 1
Points            rd 1
CurveElements     rd 1
CurveElementsLen  rd 1
PointsLen         rd 1
Threads           rd MAX_THREADS
sysInfo           SYSTEM_INFO
NullStr           rq 1