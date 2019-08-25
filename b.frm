VERSION 5.00
Begin VB.Form Form1 
   Appearance      =   0  'Flat
   BackColor       =   &H80000004&
   Caption         =   "Form1"
   ClientHeight    =   7110
   ClientLeft      =   120
   ClientTop       =   450
   ClientWidth     =   14940
   LinkTopic       =   "Form1"
   ScaleHeight     =   7110
   ScaleWidth      =   14940
   Begin VB.CommandButton Command1 
      Caption         =   "Command1"
      Height          =   495
      Left            =   6000
      TabIndex        =   14
      Top             =   5280
      Width           =   1335
   End
   Begin VB.Timer Timer1 
      Interval        =   100
      Left            =   14520
      Top             =   4440
   End
   Begin VB.PictureBox pic 
      Height          =   4935
      Left            =   0
      ScaleHeight     =   4875
      ScaleWidth      =   11955
      TabIndex        =   0
      Top             =   0
      Width           =   12015
      Begin VB.PictureBox prd 
         Height          =   375
         Index           =   2
         Left            =   1560
         ScaleHeight     =   315
         ScaleWidth      =   555
         TabIndex        =   10
         Top             =   2040
         Width           =   615
      End
      Begin VB.PictureBox prd 
         Height          =   375
         Index           =   1
         Left            =   2160
         ScaleHeight     =   315
         ScaleWidth      =   555
         TabIndex        =   9
         Top             =   2520
         Width           =   615
      End
      Begin VB.PictureBox prd 
         Height          =   375
         Index           =   0
         Left            =   1080
         ScaleHeight     =   315
         ScaleWidth      =   555
         TabIndex        =   8
         Top             =   2880
         Width           =   615
      End
      Begin VB.PictureBox prr 
         Height          =   375
         Index           =   2
         Left            =   7080
         ScaleHeight     =   315
         ScaleWidth      =   1275
         TabIndex        =   7
         Top             =   1920
         Width           =   1335
      End
      Begin VB.PictureBox prr 
         Appearance      =   0  'Flat
         BackColor       =   &H80000005&
         ForeColor       =   &H80000008&
         Height          =   375
         Index           =   1
         Left            =   2280
         ScaleHeight     =   345
         ScaleWidth      =   1305
         TabIndex        =   6
         Top             =   480
         Width           =   1335
      End
      Begin VB.Line Line2 
         BorderColor     =   &H00000000&
         Index           =   2
         X1              =   6960
         X2              =   7920
         Y1              =   4800
         Y2              =   5760
      End
      Begin VB.Line Line2 
         Index           =   1
         X1              =   2640
         X2              =   3600
         Y1              =   1920
         Y2              =   2880
      End
      Begin VB.Line Line1 
         BorderColor     =   &H00FF0000&
         Index           =   2
         X1              =   1320
         X2              =   2160
         Y1              =   1320
         Y2              =   2520
      End
      Begin VB.Line Line1 
         Index           =   1
         X1              =   0
         X2              =   840
         Y1              =   0
         Y2              =   1200
      End
   End
   Begin VB.Label Label7 
      Caption         =   "Label7"
      Height          =   375
      Left            =   8760
      TabIndex        =   13
      Top             =   6000
      Width           =   3855
   End
   Begin VB.Label Label6 
      Caption         =   "Label6"
      Height          =   255
      Left            =   8760
      TabIndex        =   12
      Top             =   5760
      Width           =   3615
   End
   Begin VB.Label Label5 
      Caption         =   "Label5"
      Height          =   255
      Left            =   8760
      TabIndex        =   11
      Top             =   5280
      Width           =   3615
   End
   Begin VB.Label Label4 
      Caption         =   "Label4"
      Height          =   375
      Index           =   2
      Left            =   8760
      TabIndex        =   5
      Top             =   6480
      Width           =   2415
   End
   Begin VB.Label Label4 
      Caption         =   "Label4"
      Height          =   375
      Index           =   1
      Left            =   720
      TabIndex        =   4
      Top             =   6360
      Width           =   2415
   End
   Begin VB.Label Label3 
      Caption         =   "Label3"
      Height          =   255
      Left            =   960
      TabIndex        =   3
      Top             =   6000
      Width           =   4215
   End
   Begin VB.Label Label2 
      Caption         =   "Label2"
      Height          =   255
      Left            =   960
      TabIndex        =   2
      Top             =   5760
      Width           =   4215
   End
   Begin VB.Label Label1 
      Caption         =   "Label1"
      Height          =   255
      Left            =   960
      TabIndex        =   1
      Top             =   5400
      Width           =   4095
   End
End
Attribute VB_Name = "form1"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Dim atkfg As Integer '攻击子弹指针
Dim sta(2), xp(2), yp(2), vy(2), vx(2), stt(2), wep(2), wes(2, 5), mload(2, 10), mnum(2), hp(2), atkpianyi(2), jitui(2) As Integer '机器人状态
Dim pos(31, 10) As Integer '地形
Dim xmax, ymax, hh, vjump, vmove, ajump, vatk, aslow, aspd, vdatk, afsbd, robr, tjump, atkpipi As Integer '常量
Dim keyw, keya, keys, keyd, key1w, key1a, key1s, key1d, key1, key3, key2, keyq, keye, keyj, keyy, keys1, keys2, keys3, keys4, keys5, keys6, keys9, keyt, keys7 As Integer
Dim keylr(2) As Integer '按键：按下左右 左负
Dim atkm(20, 8) As Integer '子弹攻击，1，xp,2,yp,3,vx,4,vy,5,6,上一个位置x,y,7,fangxiang,8,wep
Dim atkh, tatkdie, ratkdie As Integer
Dim atkdata(10, 20) As Integer '第一为枪号，第二：1 vatk 2 tcool 3 missilemax 4 tlode 5 dmg 6 悬停时间 7 耐久 8 跳跃
Dim col(3) As Long
Dim maxgun, maxguns, gunhand(2) As Integer
Dim wepdown(2) As Integer
Dim flagline(2), status1 As Integer
'特殊枪
Dim sup(2) As Integer
Dim atkdown(2) As Integer
Dim dsta(2), dxp(2), dyp(2), dvy(2), dvx(2), dstt(2), dwep(2), dwes(2, 5), dmload(2, 10), dmnum(2), dhp(2), datkpianyi(2), dkeylr(2) As Integer '机器人状态
'12.26



Private Sub Command1_Click()
If MsgBox("要开启生存模式吗？", vbYesNo) = vbYes Then hp(1) = 1: hp(2) = 1: Call lab: Exit Sub
If MsgBox("要开启吸血模式吗？", vbYesNo) = vbYes Then status1 = 1: Exit Sub
If MsgBox("要开启躲子弹模式吗？", vbYesNo) = vbYes Then
     xmax = 10
     hp(1) = 1000
     hp(2) = 1000
     xp(1) = hh * 3 / 2
     xp(2) = hh * 2
     Call prrob(1, vbBlack)
     Call prrob(2, 0)
     Call lab
     For i = 2 To (xmax - 1)
      For j = 2 To (ymax - 1)
       pos(i, j) = 0
      Next j
     Next i
     For i = 1 To ymax
     pos(xmax, i) = 1
     Next i
   pic.Cls
   Call pr
   For ss = 1 To 2
   wep(ss) = 3
   mnum(ss) = 10000
   atkdata(3, 11) = 30
   atkdata(3, 7) = 0
   wepdown(ss) = 1
   atkdata(3, 6) = 0
   atkdata(3, 8) = 500
   Next ss
   Exit Sub
End If
End Sub

Private Sub form_load()

Randomize
Show
'常数区
Open "data.txt" For Input As #1
Open "data2.txt" For Output As #2
Input #1, hh, xmax, ymax
Input #1, vjump, vmove, ajump, robr, atkpipi
Input #1, keyw, keya, keys, keyd, key1w, key1a, key1s, key1d, key1, key3, key2, keyq, keye, keyj, keyy, keys1, keys2, keys3, keys4, keys5, keys6, keys9, keyt, keys7
Input #1, atkh, maxgun, maxguns, maxatkdata, ratkdie

col(1) = vbBlue
col(2) = vbRed
col(3) = vbBlack
'图像初始化
pic.BackColor = vbWhite
pic.AutoRedraw = True
'hh = pic.Height \ ymax
pic.Move 0, 0, xmax * hh, ymax * hh
prr(1).Visible = True
prr(2).Visible = True
Line1(1).Visible = True
Line1(2).Visible = True
Line2(1).Visible = True
Line2(2).Visible = True
Label4(1) = ""
Label4(2) = ""
prr(1).Appearance = 0
prr(2).Appearance = 0
prd(1).Appearance = 0
prd(2).Appearance = 0
Line1(1).BorderColor = col(1)
Line2(1).BorderColor = col(1)
Line1(2).BorderColor = col(2)
Line2(2).BorderColor = col(2)
prr(1).BackColor = col(1)
prr(2).BackColor = col(2)
Label4(1).ForeColor = col(1)
Label4(2).ForeColor = col(2)
prr(1).ForeColor = vbWhite
prr(2).ForeColor = vbWhite
prd(0).Visible = False
prd(1).Visible = False
prd(2).Visible = False








'生成地图
For i = 1 To ymax
  pos(xmax, i) = 1
  pos(1, i) = 1
Next i
For i = 2 To xmax - 1
  pos(i, ymax) = 1
  If Rnd < 0.5 Then pos(i, ymax - 1) = 1 Else pos(i, ymax - 1) = 0
  If Rnd < 0.3 Then pos(i, ymax - 2) = 1 Else pos(i, ymax - 2) = 0
  If Rnd < 0.2 Then pos(i, ymax - 3) = 1 Else pos(i, ymax - 3) = 0
  If Rnd < 0.2 Then pos(i, ymax - 4) = 1 Else pos(i, ymax - 4) = 0
  If Rnd < 0.2 Then pos(i, ymax - 5) = 1 Else pos(i, ymax - 5) = 0

  pos(i, 1) = 1

Next i


  Call pr
'初始化变量
xp(1) = hh / 2 * 3
yp(1) = hh / 2 * 3
xp(2) = (xmax - 3 / 2) * hh
yp(2) = hh / 2 * 3
vy(1) = 0
vy(2) = 0
sta(1) = 0
sta(2) = 0
Call prrob(1, col(1))
Call prrob(2, col(2))
jitui(1) = 0
jitui(2) = 0
stt(1) = 0
stt(2) = 0
hp(1) = 1000
hp(2) = 1000
wep(1) = 1
wep(2) = 1
atkpianyi(1) = 1 * hh
atkpianyi(2) = 1 * hh
gunhand(1) = 1
gunhand(2) = 1
'第一为枪号，第二：1 vatk 2 tcool 3 missilemax 4 tlode 5 dmg 6 悬停时间 7 耐久 8 跳跃
'攻击常量
'pistol
tatkdie = 300
atkpipi = 0.2 * hh
If True Then '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  For i = 1 To maxgun
     For j = 1 To maxatkdata
        Input #1, atkdata(i, j)
     Next j
  Next i
  
  

Else


atkdata(1, 1) = 1 * hh
atkdata(1, 2) = 2 '10/5 tcool
atkdata(1, 3) = 5
atkdata(1, 4) = 5 '100\10
atkdata(1, 5) = 50
atkdata(1, 6) = 10
atkdata(1, 7) = 0
atkdata(1, 8) = 1.5 * hh
atkdata(1, 10) = 50
'第一为枪号，第二：1 vatk 2 tcool 3 missilemax 4 tlode 5 dmg 6 悬停时间 7 耐久 8 跳跃
'电光枪
atkdata(2, 1) = 0.5 * hh
atkdata(2, 2) = 3 '10/5 tcool
atkdata(2, 3) = 3
atkdata(2, 4) = 100 '100\10
atkdata(2, 5) = 25
atkdata(2, 6) = 10
atkdata(2, 7) = 50 '10000\300
atkdata(2, 8) = 1 * hh
atkdata(1, 10) = 50
'冲锋枪
'第一为枪号，第二：1 vatk 2 tcool 3 missilemax 4 tlode 5 dmg 6 悬停时间 7 耐久 8 跳跃
atkdata(3, 1) = 0.2 * hh
atkdata(3, 2) = 5 '10/5 tcool
atkdata(3, 3) = 30
atkdata(3, 4) = 2 '100\10
atkdata(3, 5) = 20 '1000\20
atkdata(3, 6) = 1
atkdata(3, 7) = 70 '10000\500
atkdata(3, 8) = 0.5 * hh
atkdata(1, 10) = 40
'第一为枪号，第二：1 vatk 2 tcool 3 missilemax 4 tlode 5 dmg 6 悬停时间 7 耐久 8 跳跃
'智能弩
atkdata(4, 1) = 3 * hh
atkdata(4, 2) = 5 '10
atkdata(4, 3) = 2
atkdata(4, 4) = 5 '100
atkdata(4, 5) = 30 '1000
atkdata(4, 6) = 1
atkdata(4, 7) = 100 '10000
atkdata(4, 8) = 1 * hh
atkdata(1, 10) = 50
'第一为枪号，第二：1 vatk 2 tcool 3 missilemax 4 tlode 5 dmg 6 悬停时间 7 耐久 8 跳跃
'镭射
atkdata(5, 1) = 20 * hh
atkdata(5, 2) = 5 '10
atkdata(5, 3) = 1
atkdata(5, 4) = 5 '100
atkdata(5, 5) = 150 '1000
atkdata(5, 6) = 1
atkdata(5, 7) = 100 '10000
atkdata(5, 8) = 0.8 * hh
atkdata(1, 10) = 50
'第一为枪号，第二：1 vatk 2 tcool 3 missilemax 4 tlode 5 dmg 6 悬停时间 7 耐久 8 跳跃
'双枪
i = 6
atkdata(i, 1) = 1 * hh
atkdata(i, 2) = 2 '10/5 tcool
atkdata(i, 3) = 5
atkdata(i, 4) = 5 '100\10
atkdata(i, 5) = 50 '1000\20
atkdata(i, 6) = 10
atkdata(i, 7) = 100
atkdata(i, 8) = 1.2 * hh
atkdata(1, 10) = 50
'第一为枪号，第二：1 vatk 2 tcool 3 missilemax 4 tlode 5 dmg 6 悬停时间 7 耐久 8 跳跃
'跟踪者
i = 7
atkdata(i, 1) = 1 * hh
atkdata(i, 2) = 2 '10/5 tcool
atkdata(i, 3) = 2
atkdata(i, 4) = 5 '100\10
atkdata(i, 5) = 50 '1000\20
atkdata(i, 6) = 100
atkdata(i, 7) = 1000
atkdata(i, 8) = 0.3 * hh
atkdata(1, 10) = 50
'第一为枪号，第二：1 vatk 2 tcool 3 missilemax 4 tlode 5 dmg 6 悬停时间 7 耐久 8 跳跃
'EMP
i = 8
atkdata(i, 1) = 1 * hh
atkdata(i, 2) = 2 '10/5 tcool
atkdata(i, 3) = 10
atkdata(i, 4) = 50 '100\10
atkdata(i, 5) = 50 '1000\20
atkdata(i, 6) = 50
atkdata(i, 7) = 20
atkdata(i, 8) = 1.5 * hh
atkdata(i, 9) = 5 * hh
atkdata(1, 10) = 50
'第一为枪号，第二：1 vatk 2 tcool 3 missilemax 4 tlode 5 dmg 6 悬停时间 7 耐久 8 跳跃
'火箭筒
i = 9
atkdata(i, 1) = 5 * hh
atkdata(i, 2) = 2 '10/5 tcool
atkdata(i, 3) = 1
atkdata(i, 4) = 3 '100\10
atkdata(i, 5) = 200 '1000\20
atkdata(i, 6) = 100
atkdata(i, 7) = 200
atkdata(i, 8) = 0.8 * hh
atkdata(i, 9) = 0.5 * hh
atkdata(1, 10) = 50
'第一为枪号，第二：1 vatk 2 tcool 3 missilemax 4 tlode 5 dmg 6 悬停时间 7 耐久 8 跳跃
'霰弹
i = 10
atkdata(i, 1) = 1 * hh
atkdata(i, 2) = 2 '10/5 tcool
atkdata(i, 3) = 15
atkdata(i, 4) = 5 '100\10
atkdata(i, 5) = 50
atkdata(i, 6) = 100
atkdata(i, 7) = 20
atkdata(i, 8) = 0.5 * hh
atkdata(1, 10) = 50
For i = 1 To maxgun
atkdata(i, 12) = 20
atkdata(i, 10) = 30
atkdata(i, 11) = 50

Next i
Print #2, hh
  For i = 1 To maxgun
     For j = 1 To maxatkdata
        Print #2, atkdata(i, j),
     Next j
     Print #2,
  Next i
End If '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


wes(1, 1) = 1
wes(1, 2) = 1
wes(1, 3) = 1
wes(2, 1) = 1
wes(2, 2) = 1
wes(2, 3) = 1
For i = 1 To maxgun
For j = 1 To 2
mload(j, i) = 10000
Next j
Next i


Call atkrenew
Call lab
sup(1) = 0
sup(2) = 0
mnum(1) = 0
mnum(2) = 0
'bugs keylr(ss) +
'pos(2, 3) = 1
'pos(29, 3) = 1
'Call pra
'robr = 1
End Sub



Private Sub timer1_timer()
For ss = 1 To 2 '竖直移动区
  yy = yp(ss) \ hh + 1
  xx = xp(ss) \ hh + 1
  Select Case stt(ss) \ 100
     Case Is = 0
        If sta(ss) = 0 Then


           vy(ss) = vy(ss) + ajump

           yy1 = yp(ss) Mod hh
           If vy(ss) > 0 Then yy2 = yy1 + vy(ss) + robr Else yy2 = (hh - yy1) - vy(ss) + robr
          ' pic.PSet (xp(ss), yp(ss) + vy(ss)), col(ss)
           yy3 = yy2 \ hh
           xx2 = xp(ss) Mod hh

           Line1(ss).X1 = xp(ss)
           Line1(ss).X2 = xp(ss)
           Line1(ss).Y1 = yp(ss)

           yp(ss) = yp(ss) + vy(ss)
           Line1(ss).Y2 = yp(ss)
           For i = 1 To yy3
            If vy(ss) > 0 Then
             If pos(xx, yy + i) = 1 Or (xx2 > (hh - robr) And pos(xx + 1, yy + i) = 1) Or (xx2 < robr And pos(xx - 1, yy + i) = 1) Then
                yp(ss) = hh * (yy + i - 1) - robr
                Line1(ss).Y2 = yp(ss)
                vy(ss) = 0
                sta(ss) = 1
                Exit For
             

               
             End If
            ElseIf vy(ss) < 0 Then
             If pos(xx, yy - i) = 1 Or (xx2 > (hh - robr) And pos(xx + 1, yy - i) = 1) Or (xx2 < robr And pos(xx - 1, yy - i) = 1) Then
                yp(ss) = hh * (yy - i) + robr
                Line1(ss).Y2 = yp(ss)
                vy(ss) = 0

                Exit For
             End If
            End If
           Next i
           
           
           Call prrob(ss, 0)
        Else
           Line1(ss).Y1 = Line1(ss).Y2
        End If

'横向
  yy = yp(ss) \ hh + 1
  xx = xp(ss) \ hh + 1
  Line2(ss).X1 = xp(ss)
  Line2(ss).Y1 = yp(ss)
  vxx = keylr(ss) * vmove
  xx1 = xp(ss) + vxx
  xx2 = xx1 Mod hh
  If sta(ss) = 1 Then '掉下去
  If xx2 < robr Then
     sta(ss) = sign(pos(xx - 1, yy + 1) + pos(xx, yy + 1))
  ElseIf xx2 > (hh - robr) Then
     sta(ss) = sign(pos(xx + 1, yy + 1) + pos(xx, yy + 1))
  Else
     sta(ss) = pos(xx, yy + 1)
  End If
  End If
  
           yy1 = xp(ss) Mod hh
           If vxx > 0 Then yy2 = yy1 + vxx + robr Else yy2 = (hh - yy1) - vxx + robr
           
           yy3 = yy2 \ hh
          
           Line2(ss).X1 = xp(ss)
           Line2(ss).Y1 = yp(ss)
           Line2(ss).Y2 = yp(ss)

           xp(ss) = xp(ss) + vxx
           Line2(ss).X2 = xp(ss)
           For i = 1 To yy3
             If vxx > 0 Then
             If pos(xx + i, yy) = 1 Then
                xp(ss) = hh * (xx + i - 1) - robr
                Line2(ss).X2 = xp(ss)
                'keylr(ss) = 0
                
                Exit For
             End If
             ElseIf vxx < 0 Then
             If pos(xx - i, yy) = 1 Then
                xp(ss) = hh * (xx - i) + robr
                Line2(ss).X2 = xp(ss)
                'keylr(ss) = 0
                
                Exit For
             End If
             

             End If
           Next i
  'xp(ss) = xx1
  'Line2(ss).X2 = xp(ss)
  'Line2(ss).Y2 = yp(ss)
  'pic.PSet (xp(ss), yp(ss)), col(ss)
  Call prrob(ss, 0)
     If stt(ss) \ 10 = 8 Then
     stt(ss) = stt(ss) + atkdata(wep(ss), 2)
     ll = robr * 2 * (stt(ss) - 80) / 10
     prr(ss).Line (robr / 2, 0)-(robr / 2, ll), vbWhite
     If stt(ss) >= 90 Then stt(ss) = 0: prr(ss).Cls 'cooldown
     ElseIf stt(ss) > 0 Then
     stt(ss) = stt(ss) + 1
     prr(ss).Line (0, robr / 4)-(robr * 2, robr / 2), vbWhite
     If stt(ss) >= 70 Then stt(ss) = 0: prr(ss).Cls
     End If
If jitui(ss) = 1 Then keylr(ss) = 0: jitui(ss) = 0

     Case Is = 1
        If stt(ss) = 100 Then prr(ss).Height = robr: prr(ss).Top = prr(ss).Top + robr
        stt(ss) = stt(ss) + atkdata(wep(ss), 10)
      
     Case Is = 2
        prr(ss).Cls
        stt(ss) = 0
        Call prrob(ss, 0)
        vy(ss) = -atkdata(wep(ss), 8)
        sta(ss) = 0
     Case Is = 3
        stt(ss) = stt(ss) + atkdata(wep(ss), 4)
        rl = robr * 2 * (stt(ss) - 300) / 100
        prr(ss).Cls: prr(ss).Line (0, 0)-(rl, rl), vbWhite
        keylr(ss) = 0
     Case Is = 4
     prr(ss).Cls
        stt(ss) = 0
        mnum(ss) = atkdata(wep(ss), 3)
        Call atkrenew
        prr(ss).Line (0, 0)-(robr, robr), col(ss)
     Case Is = 5
        stt(ss) = stt(ss) + atkdata(wep(ss), 6)
        rl = robr * 2 * (stt(ss) - 500) / 100
        prr(ss).Cls: prr(ss).Line (robr * 1.5, 0)-(robr * 1.5 - rl, rl), vbWhite
        keylr(ss) = 0
     Case Is = 6
     prr(ss).Cls
        stt(ss) = 0
        Label4(ss) = "悬停结束"
     End Select
  















Next ss

For t = 1 To atkfg
  If atkm(t, 0) = 0 Then Exit Sub 'zhuyi!!!
  Select Case atkm(t, 7) \ 1000
  Case Is = 0 'bianhua x (1 5)
   If atkm(t, 8) <> 5 Then '镭射^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
   pic.Line (atkm(t, 1), atkm(t, 2))-(atkm(t, 1), atkm(t, 6)), vbWhite '镭射^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
   Else '镭射^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
   pic.Line (atkm(t, 1), atkm(t, 2))-(atkm(t, 5), atkm(t, 2)), vbWhite '镭射^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
   End If '镭射^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
   
   vxx = atkm(t, 3)
   xx = atkm(t, 1) \ hh + 1
   yy = atkm(t, 2) \ hh + 1
   yy1 = atkm(t, 1) Mod hh
   If vxx > 0 Then yy2 = yy1 + vxx Else yy2 = (hh - yy1) - vxx
   yy3 = yy2 \ hh
   '判定撞墙
   For i = 1 To yy3
     If vxx > 0 Then
             If pos(xx + i, yy) = 1 Then
                vxx = hh * (xx + i - 1) - atkm(t, 1)
                atkm(t, 7) = 31001
                Exit For
             End If
     ElseIf vxx < 0 Then
             If pos(xx - i, yy) = 1 Then
                vxx = hh * (xx - i) - atkm(t, 1)
                atkm(t, 7) = 31001
                Exit For
             End If
     End If
   Next i
   
   atkm(t, 5) = atkm(t, 1)
   atkm(t, 1) = atkm(t, 1) + vxx
    '没有自身伤害
    ss = 3 - atkm(t, 0)
    If Abs(atkm(t, 2) - yp(ss)) < robr Then
       l1 = (atkm(t, 1) + atkm(t, 5)) / 2
       l2 = Abs(atkm(t, 1) - atkm(t, 5)) / 2 + robr
       If Abs(l1 - xp(ss)) < l2 Then
         hp(ss) = hp(ss) - atkdata(atkm(t, 8), 5)
         If status1 = 1 Then hp(3 - ss) = hp(3 - ss) + atkdata(atkm(t, 8), 5)
         If hp(ss) <= 0 Then MsgBox ("玩家" & ss & "被击毙"): hp(ss) = 1000
         keylr(ss) = keylr(ss) + atkm(t, 3) / vmove * atkdata(wep(ss), 12): sta(ss) = 0: vy(ss) = vy(ss) + atkm(t, 4) / atkdata(wep(ss), 1) * atkdata(wep(3 - ss), 8) * atkdata(wep(ss), 11): jitui(ss) = 1: sta(3 - ss) = 0
         Call lab '更新血量
         atkm(t, 7) = 31002
       End If

    End If
   
   
   pic.Line (atkm(t, 1), atkm(t, 2))-(atkm(t, 5), atkm(t, 2)), col(atkm(t, 0))
   
   
             If atkm(t, 8) <> 5 Then '镭射^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
   If atkm(t, 7) < 31001 Then atkm(t, 7) = atkm(t, 7) + 1000 '正常
             End If                 '镭射^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  
  
  
  
  
  Case Is = 1 'bianhua y (2 6)
   If atkm(t, 8) = 5 Then '镭射^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
   pic.Line (atkm(t, 1), atkm(t, 2))-(atkm(t, 1), atkm(t, 6)), vbWhite '镭射^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
   Else '镭射^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
   pic.Line (atkm(t, 1), atkm(t, 2))-(atkm(t, 5), atkm(t, 2)), vbWhite '镭射^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
   End If '镭射^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
   vxx = atkm(t, 4)
   xx = atkm(t, 1) \ hh + 1
   yy = atkm(t, 2) \ hh + 1
   yy1 = atkm(t, 2) Mod hh
   If vxx > 0 Then yy2 = yy1 + vxx Else yy2 = (hh - yy1) - vxx
   yy3 = yy2 \ hh
   '判定撞墙
   For i = 1 To yy3
     If vxx > 0 Then
             If pos(xx, yy + i) = 1 Then
                vxx = hh * (yy + i - 1) - atkm(t, 2)
                atkm(t, 7) = 31001
                Exit For
             End If
     ElseIf vxx < 0 Then
             If pos(xx, yy - i) = 1 Then
                vxx = hh * (yy - i) - atkm(t, 2)
                atkm(t, 7) = 31001
                Exit For
             End If
     End If
   Next i
   
   atkm(t, 6) = atkm(t, 2)
   atkm(t, 2) = atkm(t, 2) + vxx
   For ss = 1 To 2
    If atkm(t, 0) = ss Then

    Else
    If Abs(atkm(t, 1) - xp(ss)) < robr Then
       l1 = (atkm(t, 2) + atkm(t, 6)) / 2
       l2 = Abs(atkm(t, 2) - atkm(t, 6)) / 2 + robr
       If Abs(l1 - yp(ss)) < l2 Then
         hp(ss) = hp(ss) - atkdata(atkm(t, 8), 5)
                  If status1 = 1 Then hp(3 - ss) = hp(3 - ss) + atkdata(atkm(t, 8), 5)
         If hp(ss) <= 0 Then MsgBox ("玩家" & ss & "被击毙"): hp(ss) = 1000
         keylr(ss) = keylr(ss) + atkm(t, 3) / vmove * atkdata(wep(ss), 12): sta(ss) = 0: vy(ss) = vy(ss) + atkm(t, 4) / atkdata(wep(ss), 1) * atkdata(wep(3 - ss), 8) * atkdata(wep(ss), 11): jitui(ss) = 1: sta(3 - ss) = 0
         Call lab
         atkm(t, 7) = 31002
       End If
    End If
    End If
   Next ss

   pic.Line (atkm(t, 1), atkm(t, 2))-(atkm(t, 1), atkm(t, 6)), col(atkm(t, 0))
          If atkm(t, 8) <> 5 Then '镭射'^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
   If atkm(t, 7) < 31001 Then
   atkm(t, 7) = atkm(t, 7) - 1000 '正常
   Else
          '^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
   End If
          
          
          End If                 '镭射'^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
                If atkm(t, 8) = 7 Then '跟踪者'^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
                            ss = atkm(t, 0)
                            xx1 = (atkm(t, 1) - xp(3 - ss) - xpp)
                            yy1 = (atkm(t, 2) - yp(3 - ss) - ypp)
                            qq = Sqr(xx1 ^ 2 + yy1 ^ 2)
                            atkm(t, 3) = -atkdata(wep(ss), 1) / qq * xx1
                            atkm(t, 4) = -atkdata(wep(ss), 1) / qq * yy1
                
                End If '跟踪者'^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Case Is = 31
   pic.Line (atkm(t, 1), atkm(t, 2))-(atkm(t, 1), atkm(t, 6)), vbWhite
   pic.Line (atkm(t, 1), atkm(t, 2))-(atkm(t, 5), atkm(t, 2)), vbWhite
  If atkm(t, 7) = 31001 Then
   cc = col(atkm(t, 0))
   pic.Line (atkm(t, 1) - ratkdie, atkm(t, 2))-(atkm(t, 1) + ratkdie, atkm(t, 2)), cc
   pic.Line (atkm(t, 1), atkm(t, 2) - ratkdie)-(atkm(t, 1), atkm(t, 2) + ratkdie), cc
  ElseIf atkm(t, 7) = 31002 Then
   atkm(t, 1) = xp(3 - atkm(t, 0)): atkm(t, 2) = yp(3 - atkm(t, 0))
   cc = col(3)
   pic.Line (atkm(t, 1) - ratkdie, atkm(t, 2))-(atkm(t, 1) + ratkdie, atkm(t, 2)), cc
   pic.Line (atkm(t, 1), atkm(t, 2) - ratkdie)-(atkm(t, 1), atkm(t, 2) + ratkdie), cc
  End If
  
  
  
  If atkm(t, 8) = 8 Then 'EMP^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
     For m = 1 To atkfg
     If m <> t Then
        If Abs(atkm(m, 1) - atkm(t, 1)) < atkdata(atkm(t, 8), 9) And Abs(atkm(t, 2) - atkm(t, 2)) < atkdata(atkm(t, 8), 9) And atkm(m, 7) <= 31005 Then
           atkm(m, 7) = 31001

        End If
     End If
     Next m
     Call rec(atkm(t, 1), atkm(t, 2), atkdata(atkm(t, 8), 9), col(atkm(t, 0)))
     
  End If 'EMP^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  If atkm(t, 7) <= 31005 Then '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!31005位置
    If atkm(t, 8) = 9 Then '火箭筒^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
     For ss = 1 To 2
       If Abs(xp(ss) - atkm(t, 1)) < atkdata(atkm(t, 8), 9) And Abs(yp(ss) - atkm(t, 2)) < atkdata(atkm(t, 8), 9) Then
         hp(ss) = hp(ss) - atkdata(atkm(t, 8), 5)
         If hp(ss) <= 0 Then MsgBox ("玩家" & ss & "被击毙"): hp(ss) = 1000
         Call lab '更新血量
       End If
     Next ss
     Call rec(atkm(t, 1), atkm(t, 2), atkdata(atkm(t, 8), 9), col(atkm(t, 0)))
  End If '火箭筒^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  End If
  
  
  atkm(t, 7) = atkm(t, 7) + tatkdie
  
  
  
   Case Is = 32
   
   pic.Line (atkm(t, 1) - ratkdie, atkm(t, 2))-(atkm(t, 1) + ratkdie, atkm(t, 2)), vbWhite
   pic.Line (atkm(t, 1), atkm(t, 2) - ratkdie)-(atkm(t, 1), atkm(t, 2) + ratkdie), vbWhite
   Call rec(atkm(t, 1), atkm(t, 2), atkdata(atkm(t, 8), 9), vbWhite)
   Call pr
   For i = 0 To 8
   atkm(t, i) = atkm(atkfg, i)
   atkm(atkfg, i) = 0
   Next i
   atkfg = atkfg - 1
  End Select
Next t




'供给品区域

If xp(1) > xmax * hh / 2 And sup(1) = 0 Then Call droid(1): sup(1) = 1
If xp(2) < xmax * hh / 2 And sup(2) = 0 Then Call droid(2): sup(2) = 1


'机器人移动区域注意！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！
'机器人移动区域注意！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！
'机器人移动区域注意！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！
'机器人移动区域注意！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！
'机器人移动区域注意！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！
'机器人移动区域注意！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！
'机器人移动区域注意！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！
'机器人移动区域注意！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！
'机器人移动区域注意！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！
'机器人移动区域注意！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！
For ss = 1 To 2 '竖直移动区
 If dhp(ss) > 0 Then
  yy = dyp(ss) \ hh + 1
  xx = dxp(ss) \ hh + 1
  Select Case dstt(ss) \ 100
     Case Is = 0
        If dsta(ss) = 0 Then
           dvy(ss) = dvy(ss) + ajump
           yy1 = dyp(ss) Mod hh
           If dvy(ss) > 0 Then yy2 = yy1 + dvy(ss) + robr Else yy2 = (hh - yy1) - dvy(ss) + robr
           yy3 = yy2 \ hh
           xx2 = dxp(ss) Mod hh
           For i = 1 To yy3
            If dvy(ss) > 0 Then
             If pos(xx, yy + i) = 1 Or (xx2 > (hh - robr) And pos(xx + 1, yy + i) = 1) Or (xx2 < robr And pos(xx - 1, yy + i) = 1) Then
                dyp(ss) = hh * (yy + i - 1) - robr
                dvy(ss) = 0
                dsta(ss) = 1
                Exit For
             End If
            ElseIf dvy(ss) < 0 Then
             If pos(xx, yy - i) = 1 Or (xx2 > (hh - robr) And pos(xx + 1, yy - i) = 1) Or (xx2 < robr And pos(xx - 1, yy - i) = 1) Then
                dyp(ss) = hh * (yy - i) + robr
                dvy(ss) = 0
                Exit For
             End If
            End If
           Next i
           
           dyp(ss) = dyp(ss) + dvy(ss)
           Call prdroid(ss, 0)
        End If

'横向
  yy = dyp(ss) \ hh + 1
  xx = dxp(ss) \ hh + 1
  vxx = dkeylr(ss) * vmove
  xx1 = dxp(ss) + vxx
  xx2 = xx1 Mod hh
  If dsta(ss) = 1 Then '掉下去
  If xx2 < robr Then
     dsta(ss) = sign(pos(xx - 1, yy + 1) + pos(xx, yy + 1))
  ElseIf dxx2 > (hh - robr) Then
     dsta(ss) = sign(pos(xx + 1, yy + 1) + pos(xx, yy + 1))
  Else
     dsta(ss) = pos(xx, yy + 1)
  End If
  End If
  
           yy1 = dxp(ss) Mod hh
           If vxx > 0 Then yy2 = yy1 + vxx + robr Else yy2 = (hh - yy1) - vxx + robr
           
           yy3 = yy2 \ hh

           dxp(ss) = dxp(ss) + vxx
           For i = 1 To yy3
             If vxx > 0 Then
             If pos(xx + i, yy) = 1 Then
                dxp(ss) = hh * (xx + i - 1) - robr
                'keylr(ss) = 0
                
                Exit For
             End If
             ElseIf vxx < 0 Then
             If pos(xx - i, yy) = 1 Then
                dxp(ss) = hh * (xx - i) + robr
                'keylr(ss) = 0
                
                Exit For
             End If
             

             End If
           Next i
  'xp(ss) = xx1
  'Line2(ss).X2 = xp(ss)
  'Line2(ss).Y2 = yp(ss)
  'pic.PSet (xp(ss), yp(ss)), col(ss)
  Call prdroid(ss, 0)
     End Select
     
     
     
     
     
  End If 'hp(ss)=0
Next ss
'AI！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！
'AI！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！
'AI！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！
'AI！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！
'AI！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！
'AI！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！
'AI！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！
'AI！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！
'AI！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！
'AI！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！
'AI！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！
'AI！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！
'AI！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！
'AI！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！
'AI！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！
'AI！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！
'AI！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！
'AI！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！
'AI！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！
'AI！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！
'AI！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！
'AI！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！
'AI！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！
'AI！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！
'AI！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！
'AI！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！
'AI！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！
'AI！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！
'AI！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！
For ss = 1 To 2
  If dsta(ss) = 1 Then
     
  End If
Next ss
  











'获得武器区域！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！
'获得武器区域！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！
'获得武器区域！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！
'获得武器区域！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！
'获得武器区域！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！

For ss = 1 To 2
 jj = ss '?只能吃自己的供给品
    If Abs(xp(ss) - dxp(jj)) < 2 * robr And Abs(yp(ss) - dyp(jj)) < 2 * robr Then
       
      
       For l = 3 To 1 Step -1
         If wes(ss, l) = 1 Then
         wes(ss, l) = dwep(jj)
         mload(ss, dwep(jj)) = 10000
         Call atkrenew
         Label4(ss) = "获得枪" & dwep(jj):         flagline(ss) = 2
         sup(ss) = 0 '!!!!!
         dxp(jj) = 0
         dyp(jj) = 0
         prd(jj).Visible = False
         Exit For
         End If
       Next l
       If sup(ss) = 1 Then Label4(ss) = "枪背包满" Else sup(ss) = 1
       
    End If
Next ss

If xp(1) < 2 * hh Then sup(1) = 0
If xp(2) > (xmax - 2) * hh Then sup(2) = 0 'yingxiang


'xiaoguo
For ss = 1 To 2
  If flagline(ss) = 1 Then
     flagline(ss) = 0
     Line1(ss).X1 = xp(ss)
     Line1(ss).X2 = xp(ss)
     Line1(ss).Y1 = 0
  ElseIf flagline(ss) = 2 Then
     flagline(ss) = 0
     Line1(ss).Y1 = yp(ss)
     Line1(ss).Y2 = yp(ss)
     If Line1(ss).X1 < hh * xmax / 2 Then Line1(ss).X1 = 0 Else Line1(ss).X1 = hh * xmax
  End If
Next ss


'lianshe
For ss = 1 To 2
  If wepdown(ss) = 1 Then
  If stt(ss) < 100 Then
     If stt(ss) \ 10 <> 8 Then Call atkpress(ss, 0, 0): Label4(ss) = "连射！"
  Else
     wepdown(ss) = 0: Label4(ss) = "连射关闭"
  End If
  End If
Next ss




End Sub
Private Sub droid(ss)
dxp(ss) = hh / 2 * 3
dyp(ss) = 0
datkpianyi(ss) = 1 * hh
dwep(ss) = Fix(Rnd * (maxgun - 1)) + 2
dsta(ss) = 0
dvy(ss) = 0
dstt(ss) = 0
dhp(ss) = 100
If ss = 2 Then dxp(ss) = (xmax - 3 / 2) * hh Else dxp(ss) = 3 / 2 * hh
Call prdroid(ss, 0)
Label4(ss) = "掉落供给枪" & dwep(ss)

End Sub
Private Sub prdroid(ss, c)
If c = 0 Then c = col(ss)
prd(ss).Move dxp(ss) - robr, dyp(ss) - robr, 2 * robr, 2 * robr
prd(ss).BackColor = c
prd(ss).Visible = True
prd(ss).PSet (-robr / 2, 0)
prd(ss).FontSize = 10
prd(ss).ForeColor = vbWhite
prd(ss).Print dwep(ss)
End Sub
Private Sub pic_Keydown(KeyCode As Integer, Shift As Integer)
Select Case KeyCode
   Case Is = keyw
      
   Case Is = keya
      keylr(1) = -1
   Case Is = keys
      
   Case Is = keyd
      keylr(1) = 1
   Case Is = key1w
      
   Case Is = key1a
      keylr(2) = -1
   Case Is = key1d
      keylr(2) = 1

   End Select
End Sub
Private Sub pic_KeyUp(KeyCode As Integer, Shift As Integer)
Select Case KeyCode
   Case Is = keyw
     ss = 1
     If stt(1) < 200 Then
        If prr(ss).Height <> robr \ 1 Then stt(ss) = 100
     ElseIf stt(ss) \ 100 = 5 Then
        Call atkpress(ss, 0, -atkpianyi(ss))
     End If
   Case Is = keya
     ss = 1
     If stt(1) < 200 And stt(1) \ 10 <> 8 Then
        keylr(ss) = 0
     ElseIf stt(ss) \ 100 = 5 Then
        Call atkpress(ss, -atkpianyi(ss), 0)
     End If

   Case Is = keys
     ss = 1
     If stt(1) = 0 Then
        If sta(ss) = 1 And mnum(ss) < atkdata(wep(ss), 3) Then stt(ss) = 300
     ElseIf stt(ss) \ 100 = 5 Then
        Call atkpress(ss, atkpianyi(ss), 0)
     End If
     Call prrob(1, 0)
   Case Is = keyd
     ss = 1
     If stt(1) < 200 And stt(1) \ 10 <> 8 Then
        keylr(ss) = 0
     ElseIf stt(ss) \ 100 = 5 Then
        Call atkpress(ss, atkpianyi(ss), 0)
     End If
   Case Is = keyq '攻击
    ss = 1
    If wep(ss) = 10 Then
      If mnum(ss) <= 0 Then Exit Sub
         Call atkpress(ss, -atkpianyi(ss), 0)
         Call atkpress(ss, 0, -atkpianyi(ss))
         Call atkpress(ss, 0, atkpianyi(ss))
         Call atkpress(ss, atkpianyi(ss), 0)
         Call atkpress(ss, 0, 0)
    ElseIf wep(ss) = 6 Then
         Call atkpress(ss, 0, 0)
    End If
      Call atkpress(ss, 0, 0)

   Case Is = keye
      If stt(1) < 200 And stt(1) \ 10 <> 8 Then
          stt(1) = 500: keylr(1) = 0: vy(1) = 0
      Else
        If stt(1) \ 100 = 5 Then stt(1) = 0: Cls
      End If
   Case Is = keyy
      ss = 1
      wes(ss, gunhand(ss)) = 1
      wep(ss) = 1
      mnum(ss) = 0
      mload(ss, 1) = 10000
      Call atkrenew
      Label4(ss) = "丢失枪"
   Case Is = keys9
      ss = 2
      wes(ss, gunhand(ss)) = 1
      wep(ss) = 1
      mnum(ss) = 0
      mload(ss, 1) = 10000
      Call atkrenew
      Label4(ss) = "丢失枪"
   Case Is = keys2
      If stt(2) < 100 And stt(2) \ 10 <> 8 Then
         stt(2) = 500: keylr(2) = 0: vy(2) = 0
      Else
         If stt(2) \ 100 = 5 Then stt(2) = 0: Cls
      End If
   Case Is = key1
      If stt(1) > 300 Then Exit Sub
      wep(1) = wes(1, 1)
      mnum(1) = 0
      gunhand(1) = 1
      Call atkrenew
      Label4(1) = "换枪" & wep(1)
      
   Case Is = key2
   If stt(1) > 300 Then Exit Sub
      wep(1) = wes(1, 2)
      mnum(1) = 0
      Call atkrenew
      gunhand(1) = 2
      Label4(1) = "换枪" & wep(1)
   Case Is = key3
   If stt(1) > 300 Then Exit Sub
      wep(1) = wes(1, 3)
      mnum(1) = 0
      Call atkrenew
      gunhand(1) = 3
      Label4(1) = "换枪" & wep(1)
   Case Is = keys1 '攻击
    ss = 2
    If wep(ss) = 10 Then
      If mnum(ss) <= 0 Then Exit Sub
         Call atkpress(ss, -atkpianyi(ss), 0)
         Call atkpress(ss, 0, -atkpianyi(ss))
         Call atkpress(ss, 0, atkpianyi(ss))
         Call atkpress(ss, atkpianyi(ss), 0)
         Call atkpress(ss, 0, 0)
    ElseIf wep(ss) = 6 Then
         Call atkpress(ss, 0, 0)
    End If
      Call atkpress(ss, 0, 0)

   Case Is = key1w
     ss = 2
     If stt(ss) < 200 Then
        If prr(ss).Height <> robr \ 1 Then stt(ss) = 100
     ElseIf stt(ss) \ 100 = 5 Then
        Call atkpress(ss, 0, -atkpianyi(ss))
     End If
   Case Is = key1a
     ss = 2
     If stt(ss) < 200 And stt(ss) \ 10 <> 8 Then
        keylr(ss) = 0
     ElseIf stt(ss) \ 100 = 5 Then
        Call atkpress(ss, -atkpianyi(ss), 0)
     End If

   Case Is = key1s
     ss = 2
     If stt(ss) = 0 Then
        If sta(ss) = 1 And mnum(ss) < atkdata(wep(ss), 3) Then stt(ss) = 300
     ElseIf stt(ss) \ 100 = 5 Then
        Call atkpress(ss, atkpianyi(ss), 0)
     End If
     Call prrob(2, 0)
   Case Is = key1d
     ss = 2
     If stt(ss) < 200 And stt(ss) \ 10 <> 8 Then
        keylr(ss) = 0
     ElseIf stt(ss) \ 100 = 5 Then
        Call atkpress(ss, atkpianyi(ss), 0)
     End If
     
   Case Is = keys4
   If stt(2) > 300 Then Exit Sub
      mnum(2) = 0
      wep(2) = wes(2, 1)
      Call atkrenew
      gunhand(2) = 1
      Label4(2) = "换枪" & wep(2)
   Case Is = keys5
   If stt(2) > 300 Then Exit Sub
      mnum(2) = 0
      wep(2) = wes(2, 2)
      Call atkrenew
      gunhand(2) = 2
      Label4(2) = "换枪" & wep(2)
   Case Is = keys6
   If stt(2) > 300 Then Exit Sub
      mnum(2) = 0
      wep(2) = wes(2, 3)
      Call atkrenew
      gunhand(2) = 3
      Label4(2) = "换枪" & wep(2)
      
      
   Case Is = keyj
      ss = 1
      If stt(ss) > 0 Then Exit Sub
      If Abs(xp(ss) - xp(3 - ss)) < atkdata(wep(ss), 13) And Abs(yp(ss) - yp(3 - ss)) < atkdata(wep(ss), 13) Then
         Line1(ss).X1 = xp(ss)
         Line1(ss).Y1 = yp(ss)
         Line1(ss).X2 = xp(ss)
         Line1(ss).Y2 = yp(3 - ss)
         Line2(ss).X1 = xp(ss)
         Line2(ss).Y1 = yp(3 - ss)
         Line2(ss).X2 = xp(3 - ss)
         Line2(ss).Y2 = yp(3 - ss)
         stt(ss) = atkdata(wep(ss), 14)
         stt(3 - ss) = atkdata(wep(ss), 15)
         jitui(3 - ss) = 1
         keylr(3 - ss) = keylr(3 - ss) + sign(-xp(ss) + xp(3 - ss)) * atkdata(wep(ss), 16) / vmove: jitui(3 - ss) = 1
         vy(3 - ss) = vy(3 - ss) + sign(-yp(ss) + yp(3 - ss)) * atkdata(wep(ss), 17) * atkdata(wep(3 - ss), 8)
         sta(3 - ss) = 0
      Else
          
         flagline(ss) = 1
      End If
      Case Is = keys3
      ss = 2
      If stt(ss) > 0 Then Exit Sub
      If Abs(xp(ss) - xp(3 - ss)) < atkdata(wep(ss), 13) And Abs(yp(ss) - yp(3 - ss)) < atkdata(wep(ss), 13) Then
         Line1(ss).X1 = xp(ss)
         Line1(ss).Y1 = yp(ss)
         Line1(ss).X2 = xp(ss)
         Line1(ss).Y2 = yp(3 - ss)
         Line2(ss).X1 = xp(ss)
         Line2(ss).Y1 = yp(3 - ss)
         Line2(ss).X2 = xp(3 - ss)
         Line2(ss).Y2 = yp(3 - ss)
         stt(ss) = atkdata(wep(ss), 14)
         stt(3 - ss) = atkdata(wep(ss), 15)
         keylr(3 - ss) = keylr(3 - ss) + sign(-xp(ss) + xp(3 - ss)) * atkdata(wep(ss), 16) / vmove: jitui(3 - ss) = 1
         vy(3 - ss) = vy(3 - ss) + sign(-yp(ss) + yp(3 - ss)) * atkdata(wep(ss), 17) * atkdata(wep(3 - ss), 8)
                  sta(3 - ss) = 0
      Else
         flagline(ss) = 1

      End If
   Case Is = keyt
      ss = 1
      wepdown(ss) = 1 - wepdown(ss): Label4(ss) = "连射关闭"
   Case Is = keys7
      ss = 2
      wepdown(ss) = 1 - wepdown(ss): Label4(ss) = "连射关闭"
   End Select
End Sub
Private Sub prrob(ss, c)
If c = 0 Then c = col(ss)
prr(ss).Move xp(ss) - robr, yp(ss) - robr, 2 * robr, 2 * robr
prr(ss).PSet (-robr / 2, 0)
prr(ss).FontSize = 10
prr(ss).ForeColor = vbWhite
 prr(ss).Print wep(ss)
End Sub


Private Sub pr()
For i = 1 To xmax
  For j = 1 To ymax
    If pos(i, j) Then Call prsqr(i, j, vbGreen)
  Next j
Next i


End Sub
Private Sub prsqr(i, j, c)
Call rec(i * hh - hh \ 2, j * hh - hh \ 2, hh * 4 \ 10, c)
Call rec(i * hh - hh \ 2, j * hh - hh \ 2, hh * 3 \ 10, c)
Call rec(i * hh - hh \ 2, j * hh - hh \ 2, hh * 2 \ 10, c)
Call rec(i * hh - hh \ 2, j * hh - hh \ 2, hh * 1 \ 10, c)

End Sub
Private Sub rec(i, j, r, c)

pic.Line (i - r, j - r)-(i + r, j - r), c
pic.Line (i - r, j - r)-(i - r, j + r), c
pic.Line (i + r, j + r)-(i + r, j - r), c
pic.Line (i + r, j + r)-(i - r, j + r), c

End Sub
Private Sub recn(i, j, r, c) 'meiyouyong
r = r / 2
pic.Line (i - r, j - r / 2)-(i + r, j - r / 2), c
pic.Line (i - r, j - r / 2)-(i - r, j + r), c
pic.Line (i + r, j + r)-(i + r, j - r / 2), c
pic.Line (i + r, j + r)-(i - r, j + r), c
End Sub







Private Function sign(a)
  If a > 0 Then
     sign = 1
  ElseIf a = 0 Then
     sign = 0
  Else
     sign = -1
  End If
End Function

Private Sub lab()
Label1 = "玩家一：" & hp(1)
Label5 = "玩家二：" & hp(2)
End Sub
Private Sub atkrenew()
Label2 = "玩家一子弹数：" & mnum(1)
Label6 = "玩家二子弹数：" & mnum(2)
Label3 = "玩家一枪号：" & wep(1) & "耐久" & mload(1, wep(1)) / 100 & "% "
Label7 = "玩家二枪号：" & wep(2) & "耐久" & mload(2, wep(2)) / 100 & "%"
prr(1).Cls
prr(2).Cls
End Sub

Private Sub atkpress(ss, xpp, ypp)
      If atkfg < 20 Then
       If mnum(ss) <= 0 Then Label4(ss) = "没有子弹": wepdown(ss) = 0:       flagline(ss) = 1: Exit Sub
       If mload(ss, wep(ss)) <= 0 Then Label4(ss) = "武器失效": wepdown(ss) = 0:      flagline(ss) = 1: wes(ss, gunhand(ss)) = 1: wep(ss) = 1: mnum(ss) = 0: Call atkrenew: Exit Sub
       If stt(ss) > 0 And stt(ss) <> 80 And stt(ss) \ 100 <> 5 Then Exit Sub  '未知发射状态0？？？？？？？？？？？？？？？？？？？？？？？？？？？？？？？？？？？？
         atkfg = atkfg + 1
         atkm(atkfg, 0) = ss
         atkm(atkfg, 1) = xp(ss)
         atkm(atkfg, 2) = yp(ss)
         xx1 = (xp(ss) - xp(3 - ss) - xpp)
         yy1 = (yp(ss) - yp(3 - ss) - ypp)
         If xx1 = 0 And yy1 = 0 Then Label4(ss) = "踢吗？？？！！！": Exit Sub
         qq = Sqr(xx1 ^ 2 + yy1 ^ 2)
         
                  'If wep(ss) = 4 Then '智能弩^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
                      ll1 = atkdata(wep(ss), 1)
                      atkdata(wep(ss), 1) = qq / (qq \ atkdata(wep(ss), 1) + 1) '第一为枪号，第二：1 vatk 2 tcool 3 missilemax 4 tlode 5 dmg 6 悬停时间 7 耐久 8 跳跃
                 ' End If
         atkm(atkfg, 3) = -atkdata(wep(ss), 1) / qq * xx1
         atkm(atkfg, 4) = -atkdata(wep(ss), 1) / qq * yy1
         atkm(atkfg, 5) = xp(ss)
         atkm(atkfg, 6) = yp(ss)
                 ' If wep(ss) = 4 Then
                  atkdata(wep(ss), 1) = ll1 '智能弩^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
         If yy1 > 0 Then atkm(atkfg, 7) = 1001 Else atkm(atkfg, 7) = 1
         atkm(atkfg, 8) = wep(ss)
         mnum(ss) = mnum(ss) - 1
         mload(ss, wep(ss)) = mload(ss, wep(ss)) - atkdata(wep(ss), 7)
         Call atkrenew
         
         
         If wep(ss) = 5 Then '镭射^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
           If Abs(xx1) < Abs(yy1) Then atkm(atkfg, 7) = 1001: atkm(atkfg, 4) = -atkdata(wep(ss), 1) * sign(yy1): atkm(atkfg, 3) = 0 Else atkm(atkfg, 7) = 1: atkm(atkfg, 3) = -atkdata(wep(ss), 1) * sign(xx1): atkm(atkfg, 4) = 0  '镭射^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
         End If '镭射^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
         If wep(ss) = 6 And mnum(ss) Mod 2 = 1 Then atkm(atkfg, 7) = 1002 - atkm(atkfg, 7) '双枪^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
            stt(ss) = 80 '正常！！！！！

      End If
End Sub
