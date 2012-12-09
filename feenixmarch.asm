feenixmarch_module:
	dw @chn0,@chn1,@chn2,@chn3,@chn4,@ins
	db $06
@ins:
	dw @env_default,@env_default,@env_default
	db $30,$00
	dw @env_vol0,@env_default,@env_default
	db $30,$00
	dw @env_vol1,@env_arp0,@env_default
	db $30,$00
	dw @env_vol2,@env_default,@env_default
	db $30,$00
	dw @env_vol2,@env_default,@env_default
	db $70,$00
	dw @env_vol2,@env_default,@env_default
	db $30,$00
	dw @env_vol3,@env_default,@env_default
	db $30,$00
	dw @env_vol4,@env_default,@env_default
	db $30,$00
@env_default:
	db $c0,$7f,$00
@env_vol0:
	db $cf,$cb,$c3,$c0,$7f,$03
@env_vol1:
	db $cf,$cc,$c7,$c4,$c3,$c0,$01,$7f,$06
@env_vol2:
	db $c5,$7f,$00
@env_vol3:
	db $ca,$7f,$00
@env_vol4:
	db $c5,$7f,$00
@env_arp0:
	db $c0,$c9,$ca,$cb,$ca,$7f,$04
@env_arp1:
	db $c0,$c3,$c3,$7f,$02
@env_arp2:
	db $bb,$bb,$c0,$c0,$7f,$00

@chn0:
@chn0_loop:
@chn0_0:
	db $44,$10,$9e,$0c,$9e
	db $fe
	dw @chn0_loop

@chn1:
@chn1_loop:
@chn1_0:
	db $87,$46,$2b,$29,$80,$26,$28,$82,$2f,$82,$24,$81,$23,$23,$80,$24
	db $84,$2b,$82,$28,$29,$26,$80,$2b,$86,$2d,$86,$30,$82,$2f,$82
	db $fe
	dw @chn1_loop

@chn2:
@chn2_loop:
@chn2_0:
	db $47,$23,$9e,$1f,$9e
	db $fe
	dw @chn2_loop

@chn3:
@chn3_loop:
@chn3_0:
	db $41,$0e,$0e,$80,$0e,$0e,$80,$0e,$80,$0e,$80,$0e,$80,$0e,$0e,$0e
	db $80,$0e,$0e,$80,$0e,$0e,$80,$0e,$80,$0e,$80,$0e,$80,$0e,$0e,$0e
	db $80,$0e,$0e,$80,$0e,$0e,$80,$0e,$80,$0e,$80,$0e,$80,$0e,$0e,$0e
	db $80,$0e,$0e,$80,$0e,$0e,$80,$0e,$80,$0e,$80,$0e,$80,$0e,$0e,$0e
	db $80
	db $fe
	dw @chn3_loop

@chn4:
@chn4_loop:
@chn4_0:
	db $bf
	db $fe
	dw @chn4_loop
