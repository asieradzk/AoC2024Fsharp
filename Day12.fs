﻿module Day12

open System

let input = "RRRRIICCFF
RRRRIICCCF
VVRRRCCFFF
VVRCCCJFFF
VVVVCJJCFE
VVIVCCJJEE
VVIIICJJEE
MIIIIIJJEE
MIIISIJEEE
MMMISSJEEE"


let actualInput = "OBBBBLLLLLLVVVVVVVVVVVVVVMMMMMMMMMMMMMMMMMMMMMJJJJJJJJJJOOOOOOORRRRRRRRRRRRRRRRRRRRXXXXXXXXXXXXXXXUUUUXBXXAXXDDDDDDNNNNNNNNNNNNNNNNTTTLHHLLL
OBBBBBLLLLLLVVVVVVVVVVVVVVVVSSMMVVVMVVMMMMMMMMLJJJJJJJJJOOOORRRRRRRRRRRRRRRRRRRRRRRXXXXXXXXXXXXXXXUUXUXBXXXXXDDDDDDNNNNNNNNNNNNNNONTLLLLLLLL
OBBBBBLLLLKKKKKVVVVVVVVVVFVSSSSMMMVVVVVMMMMMMMLLJJJJJJJJJOOOOORRRRRRRRRRRRRRRRRRRRDDDXXXXXXXXXXXJXXXXXXXXXXXXDDDDDDDNNNNNNNNNNNNNNNTLLLLLLLL
OJXXXJJLLLLKKVVVVVVVVVVVVFVSSSSSMVVVVVVMMMMMMMLLJJJJJJJJOOOOOQQRRRRRRRRRRRRRRRRRRRDDPPXXXXXXXXXXJXXXXXXXXXXXXXXXDDDDDNNNNNNNNNNNNNTTLLLLLLLL
OJXXXJJJLLKKWVVVVVVVVVVFFFSSSSSMMVZVVVVVMMMMMLLLJJJJJGGGOOOOQQQRWRRRRRRRRRRRRRRRRRRRPYXXXXXXXXXXJXXXXXXXXXXXXXXXDDDDDNNNNNNNNNNNNLLLLLLLLLLL
JJXXJJJJJJJJVVVVVVGGGFFFFFSSSSSSMMVVRWWLLMMMLLLLJJJJJJGGOGOQQQQQWRRRRRRRRRRRRIRPRRPPPPPPXXXXXXXXJXMXXXXXXXXXXXXXDDDDNNNNNNNNNNNNNNEELLLLLLLL
JJJJJJJJJJJJJJJVVGGGGFFFFFFFFSSSMMMMWWWWLLMMLLLLLLJJJGGGGGGQQQQQQRRRRRRRRRIRRIPPPPPPPPPPXXXXXXXXXXMMXXXXXXXXXXXXXXDDNNNNNNNNNNNNNNNELLLLLLLL
JJJJJJJJJJJJZZZZGGGGGGGGGGGFSSSMMMMMWWWMWLLLLLLLLLJLGGGGGGGGQQQQQQQQRRRRIIIRIIPPPPPPPPPXXXXXXXCXXXMMHMXXXXXXLXXXDDDDDDNNNNFFNNMNJJLLLLLLLLLL
JJJJJJJJZZZZZZZZXGGGGGGGGGGGTGSMMMMMWWWWWLLLLLLLLLLLLGGGGMGGQQQQQQQQRRRRIIIIIIPPPPPPPPPXVVVTTXCCXXMMMMXXXXXXLLXXDDDDDDDNDNNNDNNJJJJLLLLLLLLL
JJJJJJJJJZZZZZZGGGGGGGGGGGGGGGGMMMMWWWWWWWWLLLLLLLLLLLGGGGGQQQQQQQQQQRRQGIUIIIIPPPPPPPPPVVVTXXXXXXMMMMXXXXLLLLLLDDDDDDDDDDDDDNNJJJJJJLLLLLLL
JJJJJJJJJJZZZZZGGGGGGGGGGGGGGGGGMMWWWWWWWWWWLLLLLLLLLGGGGGGQQQQQQQQQQQQQQQUIIIKKPPPPPPPPVVVVVXXXXXXMMMXXXLLLLLLDDDTTDDTDDDDDDDDJJDJJJLLLLLLL
JJJJJJJJJJZZZZZZGGGGGGGGGGGGGGGGMWWWWWWWWWWWWWLLLLLGGGGGGGGQQQQQQQQQQQQQQQIIKKKPPPPPPPPPPPVVPPPXXXXMMXXXXXLLLLLLTTTTTTTTTDDDDDDDDDJJJJZLLLLL
JJJJJJJJJJZZZZZZGGGGGGGGGGGGGGGGWWWWWWWWWWWWWLLLLLLGGGGGGGGQQQQQQQQQQQQQQQIIKYYPPNLPPPPPPPPVPPPXXXXXXXXXXXLLLLLTTTTTTTTDDDDDDGDDZDJJJJJLUUUL
JJJJJJJJJZZZZZZZZZGGGGGGGGGGGGGWWWWWWWWWWWWVVLLLLLLGGGGGGQQQQQQQQQQQQSQQYYIIIYLLPLLLLPPPPPPPPXXXXXXXXXXXXXXLTTTTTTTTTTHDDHDDDGDDDCCJJJJJUUUU
JJJJJJJJZZZZZZZZZZZGGGGGDGGGFFWWWWWWWWWWWWWVVVLLLLLGGGGGGQQQQQQQQQQQSSQYYYYIYYYLLLLLLLPPPPPPPXXXXXXXXBBXXRXTTTTTTTTTTTHHHHHHYHHRRCCJJJJJJUUU
ZJJJJJJJJZZZZZZZZZGGGGGGGUUUFFFFFWWWWWWWWWWVVVLLLLLLLLGGIQQQQQQQQQQSSYQYYYYYYYYLLLLLLLPPPPPPXXXXXXXXXXTTXTTTTTTTTTTTTIHHHHHHHHRHCCCCEJJJUUUU
ZJJJJJJJJZZZZMMZZZZGAAGGFFFUFFFFFWWWWWFFVQWVVVVVVLLLLLGGGQQQQQQQQQQQQYQYYYYYYYYYYLLLLLLLPPXXXXXWXXXXXTTTTTTTTTTTTTTTTILHHHHHHHHHEDDEEJJJUUUU
ZZZZZJJJZZZMMMMZZZZZAAGAFFFFFFFFWWWWWWWWVVVVVVVVLLLLLGGGGAAAAQQQQQQYYYYYYYYYYYYYYYLLLLLLXXXXXXXXXXSXQQTTTTTTTTTTTTTTIIIIHHHHHHHHHDDDDDUUUUUU
ZZZZZZZZZZMMMMMZZZZZAAAAFFFFFFFFFWWWWWWVVVVVVVVAALAAAGJJJAAAAAQQQQYYYYYYYYYYYYYYYYLLLLWLXXXXXXXXXXXXTTTTTTTTTTTITTITIIIDHHHHHHHHPDDDDDUUUUUU
ZZZZZZZZZMMMMMMMZMZMAAAAAFFFFFFWWWWWWWWVVVVVVVAAAAAAAGJJJJAAAADDDYYYYYYYYYYYYBBBYYLLLLWWWWXXXXXXXXXBTTTTTTTTTTTITIIIIIIDDDHHHHHDDDDDDUUUUUUU
ZZZZZZZZMMMMMMMMMMMMAAAAAAAAAAFFDWWWWSSVVVVVAAAAAAAAAAAJJJAAAAADDYYYYWYYSYYYYBBGYYYDDXXXXWXXXXXXXXXBBBTTTTTTTTIIIIIIIIIIDDDDHHHDDDDDDUUUUUUU
ZZZZZZZZZMMMMMMMMAAAAJEEEEAAAAAFDDWWLLLBBBBVAAAAAAAAAAAJJJAAAAYYYYWZWWWYSYYSYBBYYSDDDXXXXXXXXXXXXXXBBBTTTTTTTIIIIIIIIIIIDDDDDHHDDDDDUUUUUUUU
ZZIIZZZMMMMMMMMMMAAAAAEEEEEAAAAALLLLLLLLLVVVOAAAAAAAAAAJJJJAAYYYYYWWWWWWSSSSSSSSSSDDXXXXXXXXXXXXXXXBBBBTTTTTGGGOIIIIIIIIDDDDDDDDDDDDUUUUUUUV
IIIIZZZZZMMZMMMMMMMAABEEEEEAAAAAALLLLLLLLBVVVAAAAAAAAAJJJJJJYPPYYYWWUWWWSSSSSSSSSSDDDDXXXXXXXXXXXXXXBBBTBBTTGGGGIIIIIIIIDDDDDDDVVIVVVVUUUVVV
GGIIIIZZZZZZMMMMMMYAEEEEEEEEAAMMMLQLLLLLLBBVGGGAAAAAAAGGJJJJYYYYYYYYUWZWSSSSSSSSDDDDDDXXXXXXXNNXXXXBBCBBBBBTGGGCIIIIIIIIDDDDDDVVVVVVVUUUUVVV
IIIIIIIIZZMMMMMMMMYYEEEEEEJEAAMMMQQQLLLLLLBGGGGGAAASAAGGJJJJYYYYYYYWWWWSSSSSSSSDDDDDDDXXXXXXXNXXXXXBBCBBCBBCCGCCCICIIIIDDDDDDDVVVVVVVVUUUVVV
IIIIIIIIIIMMMMMMMMMYYEEEJJJJAMMMMMMLLLLLDBBBBBBGGGAAAAGGJJYYYYYYYYYYWKWSSSSSSSSSDDDDDWWWWWWXNNXXNXXBCCCCCCBCCCCCCCCIDDDDDDDDDVVVVVVVVVVVVVVV
IIIIIIIIIIMMMMMMMMYYYYYEJJJJAMMMMMMMMMBLBRBBBBGGGGGGGGGGJJJYYYYYYYYYSSSSSSSSSSSSSSDDWWWTWWWNNNNNNWCCCCCCTCCCCCQCCCCDDDDDDDDDDDVVVVVVVVVVVVVN
IIIIIIIIIIQMMMMMMMYYYYYEMMMMMMMMMMMMMBBBBBBBBBBGGGGGGGGGGJJJYYYYYYYSSSSSSSSSSSSSSSSSWWWWWWWNWNNNCCCCCCCCTCCCCCQCCCCDDDDDDDDDDDDDVVVVVVVVJVVV
IIIIIIIIIIIIMMMMMMYYYYYYYYYYMMMMMMMBMBBBBBBBBBBGGGGGGGGGGYJYYYYYYYYYSSSTSSSSSSSSKWWWWWWWWWWWWNNNNNCCCCCCCCCCCCCCCCDDDDDDDDDDDDDDVVVVVVVVVVVV
IIIIIIIIIIIIIMMMMZYYYYYYYYYYMMMMMMMBBBBBBBBBBBBGGGGGGGGGYYYYYYYYYYYYYSSTSSSSSSHSKWWWWWWWWWWWWNNNNNCCCCCCCCCCCCCCCCBDDDDDDDDDDDVVVVVVVVVVVVVV
IIIIIIIIIIIIMMMIZZZZYYYYYYYYYMMMMNBBBBBBBBBBBBGGGGGGGQQQGGYYYYYYYYYYYYYYYSSSSSSSWWWWWWWWWWWWWWNNNNCCCCCCCCCCCCCCCCBDDDDDDLDLDDVVVVVVVVVVVVVV
IIIIIIIIIIIMMZZZZZZZYYYYYYYYYYMNNNNBBBBBBBBBBBBBBGGGGQQQYYYYYLLYYYYYYYYSSSSSSSSSSWWWWWWWWWWWWWNNNNNNCCCCCCCCCCCCCCCCRRRDDLQLLDVVVVVVVVVVVVVV
IIIIIIIIIOIOMRZZZZZZYYYYYYYYYYMMNBBBBBBBBBBBBBBBGGGGGQQQYYFYBLLYYYYYYYYSNSSSSSSSSWBBHWWWWWWWSSNCNNCCCCCCCCCCCCCCCYYYRRRRLLLLLDLQVLVVVVVVVVVV
IIIIIIIOOOOOMZZZZZZZYYYYYYJJYYMMBBBBBBBBBBBBBQQBGGGQQQQQFFFBBLLLYYYLYYYSNNSBBBZSSSBBWWWHWWWWWWNCNCCCCCCUCCCCCCCCYYUYRRRRLLLLLLLLLLVVVVVVZVVV
IIIIIOIOOOOOOZZZZZZYYYYYYJJJJYYIMBBBBBBBBBBBBBQQGGGQQQQQTWBBBLLLYYYLYYYYNBBBBBBBSBBBDWWWWWWWWWCCNCCCCCCUCCCCUUUUUUUURRRLLLLLLLLLLLLVVVZVZVVV
IIIOOOOOOOOOZZZZZZZZZZYYYJJJYYYIIIIBBBBBBBBBBBQQQGGQQQQQWWBBBBLLYYYLLNNNNNNBBBBBBBBBDDWWWIIWWWCCCCCMMMMMCCCUUUUUUUUURRLLLLLLLLLLLLLVZZZHHYYY
IIIIOOOOOOOOZZZZZZZZZZYYYYYYYYITIIIIBBBBBBBBBBQQQGGQQQQQWWLBLLLLLLLLLLNNNNBBBBBBBBBBBDDDIIIWWWCCCCCMMMMMCCCUUUUUUUUURRLLLLLLLLLLLLLLLZZHHHYH
IIIIOOOOOOOOOZZZZZZZZZYYYYYYYIIIIIIIIBBBBBBBDDQQGGGQQQWWWWLLLLLLLLLLLLNNNNBBBBBBBBBBBDDDIIWWCCCCCMMMMMMMUCUUUUUUUUUIRRLLPPLLLLLLLLLLLLZHCHYH
IIIIIOOOOOOOZZZZZZZZZZYYYYIIIIIBIIIIIBBBBMCBDDDDGGGQQQWWWWLLLLLLLLLLLLLNNBBBBBBBBBBBKBBDFFWWWCCCCMMMMMMMMUUUUIUUUUUIIRLPPPPLLLLLLLLLLLHHHHHH
IIIIIOIIOOOOZZZZZZZZZYYYYYYYIIBBIIIIIBMMMMCCDDDDDDGGWWWWWWLLLLLLLLLLLLLLNBBBBBBBBBBBBBVDQFFCCCCCCMMMMMMMIIUIUUUUUIIIIIPPPPPPLLLLLLLLLLHHHHHH
IIIIIIIIIOOOFZZZZZZZZZYYYYYYBBBBBBBIIIIIICCCDDDDDDGDDDDWWWWWWLLLWWLLLGGGBBBBBBBBBBBBBBBAFFCCCCCCCMMMMMMMMIUIIUUIIIIIIQPPPPPLLLLLLLLLLLHHHHHH
IIIIIIIXIIFFFZZZZZZZZZZYYYYYBBBBBBIIIITCCCCDDDZZDDDDDZZWWWWWWWWLLWWLLLGGIIIIIBBBBBBTBBFFFFCCCCCCCCMMMMMMMIIIIIIIIIIIIQQPPPPPLLLLLLLLLLHHHHHH
IIIIIIIXIFFFFZZZZZZZZZZYYYBBBBBBBBIIIIICCPPDDZZZDDDDDZZWWWWWWWWWWWWLLGGGIIIIIBBBBOBTBFFFFFCCCCCCCCCCMMUMIIIIIIIIIIIIQQQQPPPLLLLLLHHHHHHHHHHH
IIIIIIIXIFFFFFFZZZZZZZZBBBBBBBBBBBDICCCCCCPDPZZZDDDZZZZZWWWWWWWWWWWGGGGGIIIIIBBBBTTTTTFFFFCCRRCCCCCCCMVIIIIIIIIIIIIIIQQQPPQLQLLLLHHHHHHHHHHH
IIIIIXXXIIFFFFFXXZZZZZZBBBBBBBBBBBBCCCCCCCPPPIZZZZZZZZZZZZZWWWWWWWWGGGKKIIIIIBBBTTTTTTFFTTTRRRCCCCCCZMVVIVIIIIIMMIIIIQQQQQQQQQQQLQHHHHHHHHHH
IIXXXXXXFFFFFFFFFZZZZZBBWBBBBBBBBBBCCCCCCCCPPZZZZZZZZZZZZZZWWWWWWWWWKGKKIIIIIBBTTTTTTTFFTTRRRRRCCCCVVVVVVVIIIIIMMIIIIIQQQQQQQQQQQQQQHHHHHHHE
IVXXXXXXXFFFFFFGFZZZZZBWWBBBKKBBBBNNCCCCCPPPPPPZRZZZZZZZZZWWWWWWWKKKKKKKIIIIITTTTTTTTTFFTTRRRRRCVVVVVVVVVVVIIGGMMMIIIIIIXXQQQQQQQQQHHHHHRRRE
IVVXXXXXXAFFFFFFFFZZZZZZBBBKKKKBBNNCCCCCCCPPPPPPRZZZZZZZZWWWWWWWKKKKKKKKIIIIIGTTTTTTTTTTTTXRRGGGGVVVVVVVVVVVGGGMMMMMMMIIXXQQQQQQQQQHHHRHRRRE
VVVVVXSTXTTFFFFZZZZZZZZBBKUUUUUKKCCCCCCCCPPPPPPPPZZZZZZZZWWWWWWWIIKKKKKKIIIIITTTTTTTTTTTTTXGGGGGGVVVVVVVVVVVGGGMMMMTMMXXXXQQQQQQQNNNHRRRRRRE
VVVVVVVTTTTFFFSZZZZZZZZZKKUUUUUKKCCCCCPPCPPPPPPPPZZZFFFZPPWWWWWWIIKKKKKKIIIIITTTTTTTTTTXXXXXGGGGGVVVVVVVVVVVMMMMMMMMEEXXXXOXQQQNNNNNHWRRRRRR
VVVVVVVVTTTTZFZZZZZZZZZZZZUUUUUKKKKCCCCPPPPPPPPPPZZZZFFZPPPWSWWWIKKKKKMKIIIIITTTTTTTTTXXXXXXXGGMMMMVVVVVVVVMMMMMMMMMEEXXXXXXXQQNNNNRRRRRRRRR
VVVVVVVVVTTTZZZZZZZZZZZZZZUUUUUKKKKKCCCPPPPPPPPPPZZFFFCPPPSSSSSWIKKKKKKKNNTTTTTTTTTTTTXXXXXGGGGMMMMVVVVVVVMMMMMMMMMWMXXXXXXXXXNNNNNRRRRRRRRR
VVVVVVVVVVTTZZZCCCZZZZZZZZUUUUUKKKKKOCCCPPPPPPPHPRZCCFCCSSSSSSSSIIIIIKKNNNNNNTTTTTTTTTXXXXGGGGMMBMMMMVVVVMMMMMMMMMMMMXXXXXXXXXXNNNRRRRRRRRRR
VVVVVVVVVVTTTZUFCCFZZZZZZDUUUUUKKKKKOOOIPMPPPPPPPRRRCCCCSSSSSSSSSISSSNNNNNNNNTTTTTTTTTTXGGGGGGMMMMMMMVVKMDMMMMMMMMMMXXXXXXXXXXXNNNNNRRRRRRRR
VVVVVVVVVTTTTZFFFFFWZZMMDDDDDDDDKKKOOOOOOMMPPPPPRRRCCCBCFSSSSSSSSSESSSNNNNNNNTTTTTTTTTTTAAGGGGMMMMMMMMMMMMMMMMMMMMMMGXXXXXXXXXXNNNNNRRRRRRRR
VVVVVVVTTTTTTTTFFFFFFDDDDDDDDDDDKKOOOOOOOMPPPPPPCRRRCCCCCCSSSSSSSSSSSNNNNNNNNNNNTTTTTTTTAAGGGMMMMMMMMMMMMMBMMMMMMMMMXXXXXXXXXXNNNRRRRRRRRRRR
VVVVVVVVVTTTTTTTFFFFFRDDDDDDDDDDKKOOOOOOMMMMPXXXCCCCCCCCCYSSSSSSSSSSSNNNNNNNNNNNNNNTTTTHHAGGGMMMMMMMMMMMMMMMMMMOOMMMOXXXXXXXXYXRRRRRRRRRRRRR
VDDDVVVVVTTTTTTTTBBBBRBDDDDDDDDDKOOOOOOOOOWWWWXXXCCCCCCCCCSSSSSSSSSNNNNNNNNNNNNNYNNAAATHHAAGGMMMMMMMMMMMMMMMXXOOOOOOOOOXXXXXXXXXRRRRRRRRRRRR
VDDDTTTTTTTTTTTTBBBBBBBBBDDDDKKKKKOOOOOOOZZWWWWWXXXCCCICSCSSSSSSSSSNNNNNNNNNNNYYYNNAAAAAAAAGGGMMMMMMHHMMMMXMXXOOOOOOOOXXXXXXXXXXSSRRRRRRRRRF
DDDDDTTTTTBBTTTTBBBBBBBBBDBBBKKKKKOOOOOOOOZWWWWWXXXXXKIISSSSSSSSSSLSNNNNNNNNNNYYYNNAAAAAAAAGGGMMMXMMHHMMMMXXTXOOOOOOXXXXXXXXXXXSSSRXRRRRRXRF
DDDDDTTTTTBBBBBMBBBBBBBBBBBBBKKKKOOOOOOOOOWWWWYWWXXKKKKSSSSSSSSSSSSSNNNNNYNNYYYYYYYAAAAAAAAAAGGMHXHHHMMMMMXXXXXXOQXXXXXNXXXXXHHPSSXXXXXXXXXX
DDDDDDTTTTDDDDDBBBBBBBBBBBBBKKKKKOOOOOOOOOWWWWWWWXXXXKKTTSSSSSSSSSSSNNNNNYYYYYYYYYLAAAAAAGGAAGGGHHHHHFMMZMXXXXXQQQQXQXQQXXXBXPPPPXXXXXXXXXXX
DDDDDDFFTTFSSSDBBBBBBBBBBBBBBBKKKKKOFFFOOOWWWWWWXXXXKKKKKKNSVSSSSSSSSNNNYYYYYYYYYAAAAAGGGGGGGGGGHHHHHHHZZZZXXXXQQQQQQQQQQXXBBBPPXXXXXXXXXXXX
DDDDDFFFTTFFSSSBBBBBBBBBBBBBBKKKKKKOFFFFFOFWXXWXXXXKKKKKKNNSSSSSISSSSNNNYYYYYYYYYAAAAGGGGGGGGGGGHHHHHHHZZZZXZZQQQQQQQQQQQQQQBBPPXXXXXXXXXXXX
DDDDDDDFFTFFAAVBBBBBBBBBBBBBBKBKKKKFFFFFFFFFXXXXXXXKKKKKKKNNNNNSSNSSSNNNNYYYYYYYYAYANYGYGYYYGGHHHHHHHHZZZZZZZZZQQQQQQQQQQQQQPPPPXXXXXXXXXXXX
DDDDDDFFFFFFAAAABBBBBBBBSBBBBBBWWKKQFFFFFFFXXXXXXXXXKKKKKKNNNNSSSNSSSNNLNYYYYYYYYYYYYYYYYYYYGGHEHHHHHKKZZZZZZZZQQQQQQQQQQQQPPPPPXXXXXXXXXXXX
DDDDFBFFFFFFAAAAABYBBBBBSSBXXXBWWQQQAFFFFFFXXXFFXXXXKKKKKKNNMNNNNNNNSNNNNYYYYYYYYYYYYYYYYYYGGGGHHHHHHKKKZZZZZZWQQQQQQQQQQBQPPPPPCCCCXXXXXXXX
DDDFFFFFFFFFAAAABBYYBBBBSSBXXXXXWQQQFFFFFFFXXXFFXXXXXXKKKKNMMMNNNNNNNNNNNNYYYYYYYYYYYYYYYYYGGGGHHHHHHKKZZZZZWWWQQQQWQQQWQQPPPPPPCCCXXXXXXXXX
DNDDFFFFFFFFAAAAABYYYBBBSSSXXXYXQQQQFFFFFFFFFQFFFFFFXXKKGGMMMMMFFNFNNNNNNNYYYYYYYYYYYYYYYYYGGGGGHHHHZZZZZZZZWFWQQQWWWWWWWWPPPCPPPCCXXXXXXXXC
DNUUAFFFFFFFAAAAAAYYYBBBBBXXXXXDQQQQFQQFFFQQQQFFFFFMMMGGGGMMMMMFFFFFNNNNNNNTYYYYYYYYYYYYYYPGGGGHHHHHZZZZZWWWWWWAQWWWWWWWWWMCCCCCCCXXCXXXCXXC
UUUUUFFFFFFFFAAAAYYYYYYBYYXXXDDDQQQQQQQQQFQQQQFFFFMMMMMGGMGFFFFFFFFNNNNNNHNTTTYYYYYYYYYYYYPPPPGHHHDZZZZZWWWWWWWWWWMWWWMMMMMMCCCCCCCCCXXXCCXC
UUUUFFFFFFFFFFAAAYYYYYYYYYYYXDDQQQQQQQQQQQQQXXFFMMMMMMGGMMFFFFFFFFNNNNNNNDTTTTYYYYYYYYYYPPPPPPPPNNZZZUZUUWWWWWWWWWMMMMMMMMMMCCTTTTCCCXXPCCCC
UUUUFFFFFFFFFFAAAAYYYYYYYYYYHHDQQQQQQQQQQQQQQQQMMMMMMMMGMCCFFFFFFFFNNNNDNDDDDDDCCAAYYYYYPPYYPPTTZZZZZZZZWWWWWWWWWMMMMMMMMMMMMCTNNNPPPPPPCCPC
UUUUFFFFFFFFFFFFAAYYYYYYYYYHHHDQQQQQQQQQQQQQQMMMMMMMMMMMMCCFFFFFFFFNNDDDDDDDDDDCCAAAYYYYYYYYYYTSSSSSZZSHYWWWWWWWWMMMMMMMMMMMMMKPPNPPPPPPPPPP
UUUUFFFFFFFFFFFFAYYYYYYYYYHHHHHQQKQQQQQQQQQQQQMMMMMMMMMMMMMFFFFFFFFMNDDDDDDDDDDCCCAAAAYYYYYYYYTSSSSSZZSSYYWYYYWWMMMMMMMMMMMMMMPPPPPPPPPPPPPQ
UUUUUUFFFFFFFFFFFFQQQYYYYYHHHHHKKKQQQQQQQQQQQQMMMMMMMMMMMMFFFFFFFFFFNDDDDDDDDCCCAAAAAAYYYYYYYYYSSSSSSSSSYYYYYYWWWWMMMMMMMMMMMMMMPPPPPPPPPPPQ
UUUUUUUFFFFQFFFQDQQQYYYYDHHHHHQHKKKKQQQQQQQQQQMMMMMMMMMMMUUFFFFFFFFNNNNDDDDDDCCAAAAAAAAYAYYYYYYSSSSSSSSSSYYYYYYWWQMMMMMMMMMMMMMMPPPPPPPPPPPP
UUUUUUUFFFFQQQQQQQQQDDDDDHHHHHHHKKKKQQQQQQQQQQMMMMMMMMMMMUUFFFFFFFFNNNNNDDDDDCCAZAAAAAAAAYYYYYYSSSSSSSSSYYYYYYYYYPPPPMMMMMMMMMMKPPPPPPPPPPPP
UUUUUUUFFFQQQQQQQQQQDDDDDDHHHHHHHHQQQQQQQQVQQMMMMMMMMMMMUUUUFFFFNNNNNNNNNDDCCCAAAAAAAAAAAYYYYYYYSSSSSSSSSYYYYYYYPPPPMMMMOMMMWWPPPPPPPPPPPPPP
UUUUPPUUUUUUUQQQQDDDDDDDDHHHHHHHHBBBQQQQQQVMMMMMMMMMMMMMUUUFFFFFNNNNNNNNNNDCCAAAGAAAAAAAYYYYYGBBBSSSSSSSSSYYYYPPPPPPPMMPPPMMWWWPPPPPPPPPPPPP
OUUUPPPPPIPPUQQQQLLLLDDHHHHHUUAAHBBBBBQBBBTBBBMMMMMMMGUUUUNNNFNFNNNNNNNNNNNCCCCAAAAAAAAAAYYYYBBBBSSSSSSSSSSYYYYPPPPPPPPPPPPMMWWWPPPPPPPPPMPP
OUUUPPPPPPPPUQQLLLLHDDDHHHHHUUAUUUBBBBTBBBTBBBBMMGGMGGUUUUNNNNNNNNNNNNNNNNCCCCCAAAAAAAAAAYYYYBBBSSSSSSSSSSSSYYYCPPPPPPPPPPPWWWWPPPPPPPPPPMPP
OOUPPPPPPPPPPLLLLLLHHHDDDHHUUUUUUUTTTTTBBBTTTBBBMGGGGGUUUUUNNNNNNNNNNNNNNNCCCCCCCAAAAAAAABCBBBBBSSSSSSSSCSSSSCCCPPWWPPPPPPWWWWPPPPPPPPPMMMMM
OOUOOPPPPPPPPLLLLLLLHHHHHHUUUUUUUUTTTTTTTTTTTTBBBGGGGUUUUUUNNXNNNNNNNNNNNNCCCCCCAAAAAAAAABBBBBBSSSSSSSSSCSCCCCCCCCWWPPPPPPPWWWWWWPPPPPPMMMMM
OOOOPPPPPPPPPLLLLLLHHHHHHHUUUUUUYYTTTTTTTTTTTTBGGZGGGUGGUUUNNNONNNNNNNNNNNNCCCCAAAAAAAAABBBBBBBSFFFFFFFFCSCCCCCCCCWWPPPPPPPWWWWWWPPPPPPMBMMT
OOOPPPPPPPLLLLLLLLLLLLHHUUUUUUUUUTTTTTTTTTBBBTBBGGGGGGGGGGUUNNNNNNNNNNNNNNNCCCCCAAAAAAAAABBBBBBBFFFFFFFFCCCCCCCCWWWWPPPPPPPWWWWWUUPUUPMMMMMM
OOOPPPPPPPPPLLLLLLHHLHHHUUUUUUUUUHHTTTTTTTBBBBBBGGGGGGGGGGGNNBNNNVNNNNNNNNNCCCCCCCAAAAHAAAABBBBSFFFFFFFFFFCCCCCCWWWWWWPPPWWWWWWUUUUUUMMMMMMM
OPPPPPPPPPPPPLLLLLLHHHUUUUUUUUUUUUHTTNTBBTBBBGBBGGGGGGGGGGGBBBNNNNNNNNNNCNNCCCCCCAAAAHHVABBBBBBBBFFFFFFFFFCCCCCCWWWWWPPPPPWWWWWUUUUUUMMMMMMM
YYXXPPPPPPHHHLHHLHHHHHRUFUUUUUUUHHHHTTBBBBBBBGGGGGGGGGGGGGGBBBNNNCCCCNCCCCCCCCCCCCHAAHJJJBBBBBBVCCCFFFFFFKKKKKKKKKKWWWPPPPWWUWWWUUUUUMMMMMMM
YYYYDPPYPPHHHHHHHHHHRRRRFUUUUUUUUUWWTTBBBBBGGGGGGGGGGGGGGGGBBBBCCCAPCCCPCCCCCCCHCCHHHHJJJBBBBBBVCCCCCFFFFKKKKKKKKKKWWPPPPPWWUUUUUUUUUMMMMMMM
YYYYYYYYPHHHHHHRRRRRRRRRUUUUUUUUUUWWBBBBBBBBGGGGGGGGGGGGGGGQBBQCCPPPPPCPPCCCCCCHHHHHHHJJJBBBBBBBCCCCCFFFFKKKKKKKKKKSPPPPJPWUUUUUUUUUUMMMMMMM
YYYYYYYYYHHHHHHHHRRRRRRRRUUUUUUUWWWBBBBBBXXXSGGGGGWGWGGGGGGQQQQQKPPPPPPPPCCCCCCHHHHHHHJJJHHHHHBBCCCCCFFFFKKKKKKKKKKPPPPPPWWWUUUUUUUUMMMMMMMM
YYYYYYYYYHHHHHHRRRRRRRRRRTTTUUUFWWWBBWWXXXXXSGGPWWWWWWPPGGQQQQQKKPPPPPPPPCCCCCCHHHHHHHJJJHHHHHBBCCCCCFFFFKKKKKKKKKKWPPPWWWWUUUUUUUUUMMMMMMMM
YYYYYYYYYHHHHVHRRRRRRRRRRTTTUDWWWWWWWWWZXXXXSSGWWWWWWWPPGQQQQXQKPPPPPPPPPCCCCHHHHHHHHHJJJXXHHHHXCCXCCFFFFKKKKKKKKKKWWWWWWWWWUUUUUUMUMMMMMMMM
YYYYYYYYYHHHVVHRRRRRRRRRRRTTTAWWWWWWWWWWXXXXXXTWWWWWWWWQQQQQQQQKPPPPPPPPPCCCHHAHHHHHHHJJJXXXHHXXXXXKKKKKKKKKKKKKKKKWWWZWWWWWWWWUMMMMMMMMMMMM
VYYYYYYYYVVHVRRRRRRRRRRRRRRTTAAWWWWWWWWWXXXXXXXWWWWWWWWWWQQQQDDDDDDPPPPPPCCCCHHHHHHHHHJJJXXXXHHXXXXKKKKKKKKKKKKKKKKWWWZZZWWWWWWUHMMMMMMMMMMM
YYYYYYYPPVVVVVVRRRRRRRRRRRRRTALWWWWWWWWWWXXXWWWWWWWWWWWWWQQQQDDDDDDPPPPPPPCCPHHHHHHHHXJJJXXXXXXXXXXKKKKKKKKKKKKKKKKWWZZZZZZZWWWUHMMMMMMMMMMM
YYYYYYYYPPSSSVVVRBRRRRRRRAAAAALWWWWWWWWWWWXRWWWWWWWWWWWWWQQQQDDDDDDDPPPPPPCPPHHHHHHHXXJJJXXXXXXXXXCKKKKKKKKKIIIIIIWWWZZZZZZZWZZOZMMMMFFMMMMM
TYYKYYXYPPSSWWVVBBBBBRRRAAAAAALWWWWWWWWWWQXRWWWWWWWWWWWWWQQQQDDDDDDPPPPPPPPPPHHHHHHHXXXXXXXXXXXXXXCKKKKKKKKKQQIIIIIWZZZZZZZZZZZZZOMMMMMMMMMM
TTYYBYYPPPSSSWSBBBBBBGGGAAAAAALWWWWWWWWWWWRRRWWWWWWWWWWWUUUDDDDDDDDPPPPPPPPPPPHHHHHMXXXXXXXXXXXXXXCKKKKKKKKKQLIIIIIIIVZZZZZZZZZZPPMGGGGGGMMM
TTYBBYQEPPPSSSSBBBBBBGGGAAAAALLWWWWWWWWRWRRRRWWWWWWWWWWWUUDDDDDDDDDPPPPPPPPPPPPHHHXXXXXXXXXXXXXXXXXKKKKKKKKKLLIIIIIPPZZZZZZZZZZZPPMGGGGGPMMM
TTYBBYQEEPPSSSSSBBBGGGGGAAAAAGLLLWWWWRRRRRWWWWWWWWWWWWWWZUDDDDDDDDDDPPPPPIPPPPPPPHHXXXXXXXXXXXXXWXXKKKKKKKKKLLILMIPPPZZZZZZZZZZZPPPGGGGPPPPM
TTTNNYQEPPPSSSSSBSGGGGGGGAGGAGLLWWWRRRRRRRWWWWWWWWWWUUWWZZZDDDDDDDDDDPIIIIIPPPPHHHHXXXXXXXXXXXXXWWLKKKKKKKKKLLLLMPPZZZZZZZZZZZZPPPPPPGPPYYPM
TNNNEEEESSSSSSSSSSSSGGGGGGGGGGGLVVRRRRRRRRRWWWWWWWWUUUXZZZZDDDDDDDDDIIIIIIPPPPPPEQQXXUXXXXXXXXXXWWLLLLLLLLLLLLLLMPPXXZZZZZZZZZZPPPPPPPPPPYYY
TNNNEEEESSSSSSSSSSSGGGGGGGGGGGLLVVVRRRRRRRRRWWWRWWUUUUZZZZDDDDDDDUDIIIIIIPPPPPPEEEEYXUUXXXXXKXXXWWWLLLLLLLLLLLMPPPPAXZZZZZZZZZPPPPPPPPPPPPPY
TNNNNEEESSSSSSSSSSSGGGGGGGGGGGLLLLZZZZZRZRRWWWRRUUUUUUZZZZQDDDDBBUIIIIIIIIPPPEEEEEEEEEEEXKKKKKXWWWWWWLLLLLLLLPLTTTPAAZZZZZZZZZZPPPPPPPPPPPPP
TTNNEEEEESSSSSSSSSSSGGGGGGGGGGGLLLRRZZZZZZRWRRRRUUUUUUUQZQQBBBDBBBIIIIIIIIIPPEEEEEEEEEEEECCKKKKGGWWWWLLLLLLLLLLTTTTAAAAZZZZZZZPPPPPPPPPPPPPP
TNNNEEEEESSSSSSSSSSSSGGGGGGGGGGGLRRRRZRRRRRRRRRRUUUUUUUQQQBBBBDBBIIBIIIIIIUEEEEEEEEEEEEYYKCCKKKKGWWWLLLLLLLLLEEVTAAAAAAZSSZZZZPPPPPPPPPPPPCP
TTNEEEEEESSSSSSSSJJJGGGGGGGGLGGLLRRRRZRRRRRRRRRRUUUUUUUUUQBBBBBBBBIBIIIIIIUUEEEEEEEEEEEEKKCKKKKKKKKKKLLLLLLLLLVVVAZZAAAVKKZPPPPPPPPPPPPPPPPP
ENNEEEEERSSSSSSSSSJGGGGGGGGGLGGGGURRRRRRRRRRRRRUUUUUUUUUUUBBBBBBBBBBIIIIIUUUUEEEEEEEEEEEKKKKKKKKKKKKKKKLLLLLLLVVZZZZZAAVKEPPPPPPPPPPPPPPPPPP
EEEEEEEERSSSSSSSJJJJGJGGJGLLLJJGGGRRRRRRRRRRRRRRRUUUUUUUUUBBBBBBBBBBBBIIUUUUUEEEEEEEEEYKKKKKKKKKKKKKKBBBLLLLVVVVEEZZZZAKKEPPPYPPPPPPPPPPPPPP
EEEEEEEESSSSSSSJJJJJJJJJJJJLJJGGGGGRRRRRRRRRRRRRUUUUUUUZZUBBBBBBBBBBBBIBBBUUUEEEEEEEEEYKKKKKKKKKKKKKKBKLLVVVVVVVEEEZEEAEXPPPXXXNPPPPPPPPPCPW
EEEEEEESSSISJJSJJJJJJJJJJJJJJJJGGGGGRRRRRRRRRRBUUUUUUUUZZZBBBBBBBBBBBBBBBBUCUUUEEEEDDDKKKKKKKQKKKKKKKKKVVVVVVVVVVEEEEEEEXXXXXXXXBPPPPPPPPPPW
EEEEEEEESEEJJJJJJJJJJJJJJJJJJJGGGGOGGGRRRRRRBBBBBUUUUUUUUUEBBBBBBBBBBBBBBBUCCCCEEEEEDDKKKQKQQQKKKKKKCCBVVVVVVVVVVEEEEEEEXXXXXAABBBBPMMPWWWWW
EEEEEEEESEEEJJJJJJJJJJJJJJJJJJGJGGGGGGRRRRRRBBBBBBBUUIIUEEEEBBBBBBBBBBBBFMUUCCCCCCCDDDDQCQKQUQQKKKKKCBBVVVVVVVVVVEEEEEWEXXXXXAAAAABMMMMMMWWW
EEEEEEEEEEEEJJJJJJJJJJDJDJJJJGGGGGGGGGGYYYYRBBBBBBBBBBBEEEEEEOOBBBBBBBBBMMMCCCCCCCCCDDDQCQQQQQTKKKKKKUBVVVVVVVVVEEEEEEEZXXXXXAAALMMMMMOMWWWW
EEEEEEEEEEJJJJDJDDDDDDDDDJJJJJAAGGGGYYYYYYYYYBBBBBBWWBEEEEEEEOOOOBBBRRRZZZZZZZZZZCCCCCDQCQQQQTTTKKKKKUBVVVVVVVVVEEEEEEEEUXXXXAAMMMMMMMMMMWWW
EEEEEEEEEEEJJJDJDDDDDDZDDDJJJAAAAGGGGYYYYYYYYBBBBBBBWBBEEEEOOOOOOORRRRRZZZZZZZZZZCCCDDDQQQQQQQQTTKKKKKBBBVVVVVVVEEEEEEEEUUXXXSSSSMMMNMZMWWWW
ESEESEEEEEEEEDDDDDDDDDDDDDJJAAAAAAAAYYYYYYYBBBBBBBBBBBBBBBEOOOOOORRRRRRZZZZZZZZZZCCCDDQQQQQQQQQTTKKKKBBBBBVVVVUEEEEEEEEEEXXXXXSMMMMNNNNMMWWW
ESSSSEEEEEEEEADDDDDDDDDDDJJJAAAAAAAYYYYYYYYBBBBBBBBBBBBBBBEEEOOORRRZZZZZZZZZZZZZZOCCDDQGIQQQQQQQTQQBBBBBBBVVVUUUEEEEEEEEEEXTXXSSSNNNNWWWWWWW
ESSSSSSSEEESDDDDDDDDDDDDJJJJAAAAAAAAYYYYYYYYBYBBBBBBBBBBBEEEOOOORRRZZZZZZZZZZZZZZKDDDGGGQQQQQQQQQQQBBBBBBBVUUUUUEEEEEEEEEEETXSSSNNLNNNNNNWWW
SSSSSSSSSSSSDDDHDDDDDDDDJAAAAAIAAAAAYYYYYYYYYYYBBBBBBBBEEEEEOOOORRRZZZZZZZZZZZZZZKDDGGGGQQQQQQBQQQBBBBBBNNBRUUUURRREEEEEEEETTNNNNNNNNNNNNTWW
SSSSSSSSSSSDDDDDDDDDDDDJAAAAAAAAAAAYYYYYYYYYYYBBBBBBBGGGEEEEOOOORRKZZZZZZZZZZZZZZGGGGGGGQQQQQQBBBBBBBBBBBBBRRRRRRRRREENNENNNNNNNNNNNNNNNNTWW
SSSSSSSSSSDDDDDDDDDDDDAAAAAAAAAAAAAYYYYYYYYYYBBBBBBBBGGGGEEEOOOOTRRZZZZZZZZZZZZZZKKGGGGFQQQRRQBBBBBBBBBRRRRRRRRRRRRREENNNNNNNNNNNNNNNNNNNTTT
SSSSSSSSSSDDDDDDDDDDDDAAAAAAAAAAAAAAYYYYYYYYYYBBBBBGBGGGGEEEEOTTTTRZZZZZZZZZZZZZZZKGGGGFQQRRRQBBBBBXBBXRRRRRRRRRRRRNNENNNNNNNNNNNNNNNNNTTTTT
SSSSSSSSSSDDDDDDDDDDDDAAAAAAAAAAAAAYYYYYYYYYYJBBBBBGGGGGGEEEETTTTTTZZZZZZZZZZZZZZZKKKKFFQBBBBBBBXXXXXXXXXRRRRRRRRRNNNENNNNNNNNNNNNNNNNNNTTTT
SSSHSHSSHSSDDDDHHHHDDDAAZAAAAAAAAYYYYYYYYYYYYNXBBBBGGGGGGGETTTTTTTTTEEVVZZZZZZZZZZKKKKFFBBBBBBBXXDXXXXXXXXXRRRRNNRNNNNNNNNNNNNNNNNNNNNTTTTTT
SSHHHHHHHSSDDHHHHHBHDHHAAAAAAAAAAYYYYYYYYYYYYNXNIBIIGGGGGGTTTTTTTTTTEVVVZZZZZZZZZZKKFFFBBBBBBBBXXXXXXXXXXXXRRRRNNNNNNNNNNNNTNNNNYNTTNNTTTTTT
SSSHHHHHHSSSDHHHHHHHHHHCCAAAAAACCYYYYYYYYYYNYNNNIIIIGGGYGGFTTTTTTTTVVVVVVVVKKKKKKKKKFBBBBBBBBBBBXXXXXXXXXUXRRRRRNNRRNNNNNNNTNNNNTNTNNNTTTTTT
SSSHHHHHHHHSHHHHHHHHHHHHCAAAAAACCYYYYYNYYYYNNNNPPPIGGGGGGGGTTTTTTVTVVVVVVVVKKVKKKKKKFFBBBBBBBJXBXXXXXXXXXXXRNNNRRNNRNNNNNNRTNNNNTNTNTNTTTTTT
SSHHHHHHHHHHHHHHHHHHHHHCCCCAACCCCCNWNNNNYYYNNNNNPPPPGGGIIITTTTTTVVVVVVVVVVVVVVKKKKKKKFBBBBBBBJXXXXXXXXXXXXXNNNNNRRRRNNNNNNRTTTTTTTTTTTTTTTTT
HHHHHHHHHHHHHHHHHVHHVVCCCCCCCCCCCCNNNNNNNNNNNNNNPPPPPPPIIIITITVVVVVVVVVVVVVVVVKKKKKKKBBBBBBBOBXXXXXXXXXXXXXNNNNNRNRRNNNNNRROOTTTTTTTTTTTTTTT
HHHHHHHHHHHHHHHHHVVVVVCCCCCCCCCCCCNKNNNNNNNNNPPPPPPPPPPIIIIIIIIVVVVVVVVVVVVVVKKKKKKKBBBBBBBBBBBXXXXXXXXXONNNNNNNNNRRNNNNNNROOTTTTTTTTTTTTTTT
HHHHHHHHHHHHHVVVVVVVFVVVCCCCCKKKCNNKNNNNNNNNNPPPPPPPPPPPPIPIIIIVVVVVVVVVVVVVKKKKKKKBBBBBBBBBBBBXXXXXXXXXXNNNNNNNNNNRRNNNNNNOOTTTTTTTTTTTTTTT
HHHHHHHHFFHFVVVVVVVVVVVCCCCCCKKKKNNNNNNNNNNNNNPPPPPPPPPPPPPIIIIVIVUVVVVVVVVVKKKKKKBBBBBBBBBBBBBBXXXXXXXXNNNNNNNNNNNNNBNOONNOOOTZZTTTTTTTTTTT
HHUHHHHHFFFFVVVVVVVVVVVVCCCCCKKKMNNNNNNNNNNNNNNPPPPPPPPPPPPIIIIIIIVVVVVVVVVVKKKKKKBBBBBBBBBBBBPBPPXXPPPXNNNNNNNNNNNOOOOOOOOOOOOOZZZZZTTTTTTT
HHHFFFHHFFFFVVVVVVVVVVVCCCCCCKKKKXNNNNNNNNNNNNNNPPPPPPPPPPIIIIIIIIIIVVIVVIIKKKKKKGGGBBBBBBBBBBPPPPXXXPNNNNNNNNNNNNNNOOOOOOOOOOZZZZZZZTTTTTTT
FFFFFFHHFFFFFFVVVVVVVVCCCCCKKKKKKKKNNNNNNNNNNNPPPPPPPPPPPPIIIIIIIIIIVIIIIIIIKKKKGGGGBBBBBBQBBBPPPPXXPPNNBBNNNNNNNNNNNNWOOOOOOOEEEZZZZTTTTTTT
FFFFFFFFFFFFFFFVVVVVVVCCCCKKKKKKKHKKKKWNNNNNNNPPPPPPIIIIIIIIIIIIIIIIIIIIIIIIKKKGGGGGBPPBHBQBBPPPPPPPPNNNBBNNNNNNNNNNNNOOOOOOOOOEEEZZZTTTTTTT"

type Tile ={
    x: int
    y: int
    plant: char
}

type Grid = Tile[,]

let parseInput (input: string) =
    let lines = input.Trim().Split([|"\r\n"; "\n"|], System.StringSplitOptions.None)
    let height = lines.Length
    let width = lines[0].Length
    
    let grid = Array2D.init height width (fun y x ->
        let plant = lines[y].[x]
        { x = x; y = y; plant = plant;}
    )
    grid

let grid = parseInput input

type Region = {
    tiles: Tile list
    plant: char
}


let isInContact (tile1: Tile) (tile2: Tile) =
    let dx = abs (tile1.x - tile2.x)
    let dy = abs (tile1.y - tile2.y)
    tile1.plant = tile2.plant && 
    ((dx = 1 && dy = 0) || (dx = 0 && dy = 1))

let canBeAddedToRegion (tile: Tile) (region: Region) =
    region.plant = tile.plant && 
    region.tiles |> List.exists (fun regionTile -> isInContact regionTile tile)

let findRegions (grid: Grid) =
    let height = Array2D.length1 grid
    let width = Array2D.length2 grid
    
    let rec processCell (x: int) (y: int) (regions: Region list) =
        if y >= height then regions
        elif x >= width then processCell 0 (y + 1) regions
        else
            let currentTile = grid[y, x]
            
            // Find ALL regions this tile could connect to
            let connectingRegions = 
                regions 
                |> List.filter (canBeAddedToRegion currentTile)
            
            match connectingRegions with
            | [] -> 
                // No connecting regions - create new one
                let newRegion = { tiles = [currentTile]; plant = currentTile.plant }
                processCell (x + 1) y (newRegion :: regions)
            | connectedRegions ->
                // Merge all connecting regions and add current tile
                let mergedTiles = 
                    currentTile :: (connectedRegions |> List.collect (fun r -> r.tiles))
                let mergedRegion = { tiles = mergedTiles; plant = currentTile.plant }
                let otherRegions = 
                    regions |> List.filter (fun r -> 
                        not (List.contains r connectedRegions))
                processCell (x + 1) y (mergedRegion :: otherRegions)
    
    processCell 0 0 []




let regions = findRegions grid

let getArea (region: Region) = 
    region.tiles.Length



let getPerimeter (region: Region) =
    let hasTileAt x y = 
        region.tiles |> List.exists (fun t -> t.x = x && t.y = y)
    
    region.tiles
    |> List.sumBy (fun tile ->
        // Count empty adjacent spaces (up, right, down, left)
        [(0,-1); (1,0); (0,1); (-1,0)]
        |> List.sumBy (fun (dx, dy) ->
            if hasTileAt (tile.x + dx) (tile.y + dy) then 0 else 1
        )
    )

let computeValue (region: Region) =
    let area = getArea region
    let perimeter = getPerimeter region
    area * perimeter


let total = regions |> List.sumBy computeValue
printfn "Total: %d" total

//pt2
