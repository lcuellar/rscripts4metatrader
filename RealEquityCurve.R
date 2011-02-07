#  THE LICENSED WORK IS PROVIDED UNDER THE TERMS OF THE ADAPTIVE PUBLIC LICENSE ("LICENSE") AS FIRST COMPLETED BY: XMPH.FOREX@GMAIL.COM, as verified on http://pgpkeys.pca.dfn.de/ as identified by 
#  THE BELOW PGP PUBLIC KEY BLOCK, aNY USE, PUBLIC DISPLAY, PUBLIC PERFORMANCE, REPRODUCTION OR DISTRIBUTION OF, OR PREPARATION OF DERIVATIVE WORKS BASED ON, THE LICENSED WORK CONSTITUTES RECIPIENT'S 
#  ACCEPTANCE OF THIS LICENSE AND ITS TERMS, WHETHER OR NOT SUCH RECIPIENT READS THE TERMS OF THE LICENSE. "LICENSED WORK" AND "RECIPIENT" ARE DEFINED IN THE LICENSE. A COPY OF THE LICENSE IS LOCATED IN THE 
#  TEXT FILE ENTITLED "LICENSE.TXT" ACCOMPANYING THE CONTENTS OF THIS FILE. IF A COPY OF THE LICENSE DOES NOT ACCOMPANY THIS FILE, A COPY OF THE LICENSE MAY ALSO BE OBTAINED AT THE FOLLOWING 
#  WEB SITE: http://code.google.com/p/rscripts4metatrader/
#    
#	Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for the specific language governing rights 
#	and limitations under the License.
#
# -----BEGIN PGP PUBLIC KEY BLOCK-----
# Version: SKS 1.0.10 
#
# mQGNBE08+ssBDADB7T8lp4EWRTckcmiINkhZ/FGAAgJIG5J1zvEu43YDRlYHU9rD8w0MGeXI
# 7xjM5HZdNhk6d1QowVDKGxb+3YEkpkcnlwH0DPXJJZLM6yxEN165VHDqI8S21kd/o6/PkgL+
# rgE3PMHQPuTVyCyb2iroLgAIFripB5L1Uu4oxwywuf/OCn8PnLiKOGdrvsIrFRkwe/yiiT+r
# o0JKEvUv6VAaJ2o5gydtMI0uhdopXIgs/2rBhGIAQIyUgvsQ86VCiJxlUPD1AwNEYnW4Dthl
# SSev5ppmHKlxCIXglFmE4TkzMKaCTlIxnsLuHV96N8s/O0FK7zYWqa/W5daHAzj3+CuaiC6b
# 7RRw/7mknBrGPl/MaQa/fyUPlrNhxvPLxVmMNf44E8heSW5gfUBYJ8xdZLTp65YtDRpCDv/6
# hUMv1ocdWRCCyefui05pWQ7BdgUAUMxvZbWDObgGu7JiWduAq+33qUPxAC4Ny3fKPgztm1lg
# Y5h0sZ2mT8rT+DthAZNzOysAEQEAAbRAeG1waC5mb3JleCAoUlNBMzA3MmJpdHMgLSBObyBl
# eHBpcnkgZGF0ZSkgPHhtcGguZm9yZXhAZ21haWwuY29tPokBuAQTAQIAIgUCTTz6ywIbLwYL
# CQgHAwIGFQgCCQoLBBYCAwECHgECF4AACgkQF6majR3vl7uelwwAl0ViVEQlq8JOYS8q1V+x
# QN6jo5F6K4OZl1Y5cD3OQsMOmbyKQeeB34Tfgfrbp1HWi/EjH12nmsb7ceIzFoLMSFEGKF0P
# CgSZ3tlJTtO+fOCRSY9+/vjwqBKQo0A3HiGgtuBK9mHwO1B8gTGR7LHbcqplkigiF/dpwoeV
# wMIOH5EgOnwftiMIgwIrje5xUGLIZ8dBvPsWzgge6EVkLi/mwkn7mJL/9rl62JVEzO1eWn2r
# g68oXKGk6FDF98VMpeaF34tOmVeelJE2bU9Vd292tpkUUa1+C7P7hMeyTnb17OufsnGeUYNr
# DvUSZn9UDQcNoVl0EMmLUvta+6m3Ge0P4jhMTubDEQOlkBRFZSseGfCU5t6H3hM0JbrcWMDu
# ftyn3HNGY6GuK0rcIrZbQtIF/n7P63ghSYP/9JvKFqexjN1eTN3FIGmpDz+KWjiqrOuda+5A
# 5pfRH6vvGRbbtxAHr/FeNpa4sPa5b7BeeB3MyvEt8XPr3RGDwIW20ikypmQ5
# =mL7c
# -----END PGP PUBLIC KEY BLOCK-----

MT4_RealEquityCurve_Analysis = function (BackHtm, PaperHtm, Title)
{
	# Input for the backtesting period
	Back = MT4_ParseStrategyReport(BackHtm)
	Back_TS = MT4_CreateTimeSeries_FromStratRep(Back)

	# Input for the paper trading period
	Paper = MT4_ParseStrategyReport(PaperHtm)
	Paper_TS = MT4_CreateTimeSeries_FromStratRep(Paper)

	# Displaying both period on chart for Equity Analysis
	B = zoo(as.numeric(as.character(Back_TS$Balance)), as.chron(Back_TS$timeBalance) )
	P = zoo(as.numeric(as.character(Paper_TS$Balance)), as.chron(Paper_TS$timeBalance) )

	# ploting results
	MinTime = min(as.chron(Back_TS$timeBalance), as.chron(Paper_TS$timeProfit))
	MaxTime = max(as.chron(Back_TS$timeBalance), as.chron(Paper_TS$timeProfit))

	# Test for TRANSPARENCY" plot(B, col=rgb(0,0,0,50,maxColorValue=255), lwd=5, , pch=16); lines(B,col=rgb(255,0,0,50,maxColorValue=255),lwd=10)
	
	# I'm still looking for the best way to render these ... I would like
	# to have a separate background for the Paper-period, or orverlay 
	# a rectangle on the period.

	plot(B, col="blue", ylim=c(min(B,P), max(B,P)), xlim=c(MinTime, MaxTime), main=Title )
	lines(P, col="red")
	legend("topleft", c("Backtesting","Paper-Trading"), pch=3, col=c("blue", "red"))

	#par(mfrow=c(1,2))
	#plot(B, col="blue", ylim=c(min(B,P), max(B,P)))
	#plot(P, col="red", ylim=c(min(B,P), max(B,P)))

	#split.screen(c(1,2))
	#screen(1)
	#plot(B)
	#screen(2)
	#plot(P)

}