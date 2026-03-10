Practical examples
------------------

The REST API is complex interface that is difficult to operate out of the box without any prior familiarization. To
give a system integrator a good starting point, the contrib directory contains "tryme-scripts" that serve as examples
and an easy way try out the REST API.

### Scripts

There is a tryme script to `create` downloads (tryme_download.sh) and one tryme script per PSMO/eCO. The scripts are
called with a one letter parameter that refers to a profile that is pre-configured in tryme.cfg (see below). To get
an overview which profiles are available/preconfigured, a "tryme_*.sh" script may be called without parameters.

It should be noted that the tryme_*.sh scripts are really just simple examples that were created to simplify testing
during development.

#### restop.py

The python-script "restop.py" is called by the tryme "tryme_*.sh" scripts. This script can be used as a stand-alone
tool to operate the REST API. The intended usecase of "restop.py" is to offer an easy access to the REST API for
testing and development. It should be noted that this tool can not replace a proper eUICC management backend. A REST API
user must keep track of the REST resources he created, monitor them, check for errors, delete REST resources, resubmit
REST resources in case an `order` has failed, etc.

#### tryme.cfg

This is just a simple shellscript that sets some initial variables. Some sample values are already present. It is
recommended to edit tryme.cfg and to replace the sample values with some useful values. This is in particular the
$EID variable at the top of the file.

The file also defines some sample profiles along with their activation codes (`$AC`) and ICCIDs (`$ICCID`). The ICCID is
usually not known in advance. It becomes known after the eUICC has decrypted and installed the profile package. It
should also be noted that the ICCID parameter is always issued in its raw format (digits swapped, padded with 'F' at
the end).

In tryme.cfg one will also find a profile `X`, this profile is a placeholder in case the user decides not to edit
tryme.cfg and to pass all parameters from the commandline instead.

### Downloading And Enabling A Profile

In the following we will discuss how a profile download is triggered and how the results should look like. In the
following example we will pass all parameters directly from the commandline. It is assumed that the REST API of
onomondo-eim is available at 127.0.0.1:8080.

In the first step we will issue a download `order` using the `tryme_download.sh`. The parameter `X` tells tryme.cfg to
use the placeholder profile. The second parameter is the EID (not to be confused with ICCID) of the eUICC. The
third parameter serves as a placeholder for the ICCID, which we do not know or need yet. The last parameter is the
`activationCode`.

```
./tryme_download.sh X 12345678900000000000000000001234 NOT_NEEDED '1$rsp.example.com$EXAMPLE'
```

When the script is executed, it will `create` the related REST resource and then `lookup` the REST `resource`
periodically. As soon as the IPAd fetches the eIM package with the ProfileDownloadTriggerRequest we should see the
`status` change from `new` to `work` and when the download is done, the status should change again to `done`.

When the profile download was successful, the JSON output should look like this:

```
{"status": "done", "timestamp": "1718710695", "resource": {"eidValue": "12345678900000000000000000001234", "order": {"download": {"activationCode": "1$rsp.example.com$EXAMPLE"}}}, "outcome": [{"profileInstallationResult": {"finalResult": "successResult", "iccid": "12324567899999911191"}}], "debuginfo": "1234...ABCD..."}
```

The profile download was successful in case the `outcome` shows a `successResult` along with the `iccid` value. This
then means that the profile is successfully installed and that we may abort the `tryme_download.sh` script now.
(to keep the rest table clean one should `delete` the rest resource now as described above)

However, the profile is not enabled yet. In order to use it, we must issue an `enable` PSMO first. To do that we may
run `tryme_enable.sh`. The parameter `X` tells tryme.cfg to use the placeholder profile again. The second parameter is
the EID and the third parameter is the ICCID that we have just taken from the JSON output above.

```
./tryme_enable.sh X 12345678900000000000000000001234 12324567899999911191
```

We now must wait again until the IPAd fetches the related eIM package with the eUICC package that contains the `enable`
PSMO. When the status reaches `done`, we should see the following JSON output:

```
{"status": "done", "timestamp": "1718710734", "resource": {"eidValue": "12345678900000000000000000001234", "order": {"psmo": [{"enable": {"iccid": "12324567899999911191", "rollback": false}}]}}, "outcome": [{"enableResult": "ok"}], "debuginfo": "1234...ABCD..."}
```

The enableResult in the outcome shows `ok`. This means that everything went well and the profile is now enabled.

### Getting a List with Installed Profiles

The PSMO `listProfileInfo` (see also GSMA SGP.32, section 5.13.4) allows the REST API user to get a list of all
currently installed profiles, including their ICCIDs. This info is in particular valuable in case the API user must
synchronize its local records with the actual situation on the eUICC.

To send a `listProfileInfo` run:
```
./tryme_listProfileInfo.sh X 12345678900000000000000000001234
```

As soon as the IPAd has fetched and executed the related eUICC package, the JSON output should look like this:

```
{"status": "done", "timestamp": "1721306238", "resource": {"eidValue": "12345678900000000000000000001234", "order": {"psmo": [{"listProfileInfo": {}}]}}, "outcome": [{"listProfileInfoResult": {"finalResult": "successResult", "profileInfoList": [{"iccid": "989444999999990920F3", "isdpAid": "A0000005591010FFFFFFFF8900001000", "profileState": "enabled", "serviceProviderName": "OsmocomSPN", "profileName": "TS48V1-A-UNIQUE", "profileClass": "operational"}, {"iccid": "989444999999990930F1", "isdpAid": "A0000005591010FFFFFFFF8900001100", "profileState": "disabled", "serviceProviderName": "OsmocomSPN", "profileName": "TS48V1-B-UNIQUE-nojavacard-nocsim", "profileClass": "operational"}]}}], "debuginfo": "1234...ABCD..."}
```

We can see that there are two profile installed `989444999999990920F3` and `989444999999990930F1`. We also can see that
`989444999999990920F3` is currently enabled.

### Performing an eUICC Data Request

The eUICC data Request is a special operation, that allows the eIM to request some important master data from the eUICC.
(see also GSMA SGP.32, section 2.11.1.2) This includes the EUM and the eUICC certificate that is required to
authenticate eCO and PSMO responses. The order takes a `tagList` as input, which describes what kind of data to request.
The exact meaning of the tags can be found in the aforementioned spec reference.

To perform an eUICC data request, execute the following script like so:
```
./tryme_euiccDataRequest.sh X 12345678900000000000000000001234
```

There should be response like this:
```
{"status": "done", "timestamp": "1721307295", "resource": {"eidValue": "12345678900000000000000000001234", "order": {"edr": {"tagList": "80BF20BF228384A5A688A9BF2B"}}}, "outcome": [{"euiccDataResult": {"edrResult": "ok", "euiccData": {"euiccInfo1": "BF20618203020500A92C0414F54172BDF98A95D65CBEB88A38A1C11D800A85C30414C0BC70BA36929D43B467FF57570530E57AB8FCD8AA2C0414F54172BDF98A95D65CBEB88A38A1C11D800A85C30414C0BC70BA36929D43B467FF57570530E57AB8FCD8", "associationToken": 1, "eumCertificate": "308202783082021FA003020102020412345678300A06082A8648CE3D04030230443110300E06035504030C07546573742043493111300F060355040B0C0854455354434552543110300E060355040A0C0752535054455354310B30090603550406130249543020170D3230303430313039323833375A180F32303534303332343039323833375A3037310B300906035504061302455331153013060355040A0C0C52535020546573742045554D3111300F06035504030C0845554D20546573743059301306072A8648CE3D020106082A8648CE3D030107034200041330D59256AC0CB50BD928D0F4C68007C485FE3F42988AD3EE3875AE33F4983AB23B4DD4C31340D676DD8E11F9C5CBA1B11EB694EED0994DB529285E632C8906A382010830820104301F0603551D23041830168014F54172BDF98A95D65CBEB88A38A1C11D800A85C3301D0603551D0E04160414DD3DA24D350C1CC5D0AF0965F40EC34C5EE409F1300E0603551D0F0101FF04040302020430170603551D200101FF040D300B3009060767811201020102300E0603551D1104073005880388370530120603551D130101FF040830060101FF02010030350603551D1F042E302C302AA028A0268624687474703A2F2F63692E746573742E6578616D706C652E636F6D2F43524C2D422E63726C303E0603551D1E0101FF04343032A030302EA42C302A31153013060355040A0C0C52535020546573742045554D3111300F060355040513083839303439303332300A06082A8648CE3D040302034700304402200C567BF01E45244863AD7A4613F7572EEF3439F698B4711AA397AEEFC5445CE702206E993AA0A505F260B0EEF62CC30A2BBE453B0E8248218FD53304EF7F9074EE10", "euiccCertificate": "30820217308201BDA0030201020209020000000000000001300A06082A8648CE3D0403023037310B300906035504061302455331153013060355040A0C0C52535020546573742045554D3111300F06035504030C0845554D20546573743020170D3234303530313133313733305A180F37353030303232333133313733305A307C310B3009060355040613024445311E301C060355040A0C157379736D6F636F6D2052535020546573742045554D312930270603550405132038393034343034353131383432373438343830303030303030303031313632383122302006035504030C197379736D6F45554943432D49325420546573742065554943433059301306072A8648CE3D020106082A8648CE3D03010703420004BD2C55B28B4E801CA0B14F195788FD86C7FF49764C88A2934C69594A26FF647549F3F16060BD9C8D793D95D0126FA429C94966FE7842967263795A73C498F1DBA36B3069301D0603551D0E04160414E24191C0ECCEA8FC45926196EF1A2E53D5E6CD06301F0603551D23041830168014DD3DA24D350C1CC5D0AF0965F40EC34C5EE409F1300E0603551D0F0101FF04040302078030170603551D200101FF040D300B3009060767811201020101300A06082A8648CE3D0403020348003045022100E3BF7BAF5106E51656E315B7FED1D6F8360CA0F4D70ECB8D5AD38E159DE4A9C702203C3B11B748EC4788D8C62A6D75C13B0959CB56A8C2D68DD9C7C66C0615AACE73", "ipaCapabilities": "300B8005000001000181020001", "deviceInfo": "3008800412345678A100", "notificationsList": "A0819CBF378198BF27528010E0F1356728A44720B219A3B70C9DF57EBF2F2E800102810207800C1974657374736D6470706C7573312E6578616D706C652E636F6D5A0A989444999999990930F1060388370AA208A1068001028101095F37407FA95E72677338D953EE62395D7BCDC365BF217DFE6DAF315A98503F9DEF97103EAFBA7EDB5E16621BA14061354A75390FF0E8332B26AC31C8E2C9FAA6C07358"}}}], "debuginfo": "1234...ABCD..."}
```

Since we used the tagList "80BF20BF228384A5A688A9BF2B", which includes all possible tags to request, we get a lot of
information back. The returned data fields are in their ASN.1 encoded representation unless the requested tag consists
of a single primitive type (e.g. `associationToken`).

### Setting a Parameters in the `euicc` Table

Even though the `euicc` table is populated automatically, it may still be that the REST API user wants to adjust
certain parameters. Let's assume that we have a setup that mostly uses consumer eUICCs in an IoT emulation mode. Now we
want to add a native IoT eUICC. Let's say the card uses a root CI that we do not have configured in `sys.config` yet,
but we have the public key. Also the card has been used for experiments with PSMOs already, so the counterValue is
somewhere in the upper three digits range and not at 1 as it would be for a virgin eUICC.

In this case we would craft an order like this:
```
{ "eidValue" : "'$EID'", "order" : { "euicc": [ { "counterValue" : 1000 }, { "consumerEuicc" : false }, { "signAlgo" : "prime256v1" }, { "signPubKey" : "04BD2C55B28B4E801CA0B14F195788FD86C7FF49764C88A2934C69594A26FF647549F3F16060BD9C8D793D95D0126FA429C94966FE7842967263795A73C498F1DB" } ] } }'
```

We would edit the order into tryme_set_euicc_param.sh and run the script:
```
./tryme_set_euicc_param.sh X 12345678900000000000000000001234
```

The order will execute as an internal process. This means that no IPAd interaction is involved. However, on the REST
API the behavior will not be any different, except that the `status` will change from `new` to `done` directly. When
all changes to the `euicc` table are made accordingly, we should get a result like this:

```
{"status": "done", "timestamp": "1721309044", "resource": {"eidValue": "12345678900000000000000000001234", "order": {"euicc": [{"counterValue": 1000}, {"consumerEuicc": false}, {"signAlgo": "prime256v1"}, {"signPubKey": "04BD2C55B28B4E801CA0B14F195788FD86C7FF49764C88A2934C69594A26FF647549F3F16060BD9C8D793D95D0126FA429C94966FE7842967263795A73C498F1DB"}]}}, "outcome": [{"euiccUpdateResult": "ok"}], "debuginfo": "836400046E6F6E65"}
```

Now the eIM will treat the eUICC as a native IoT eUICC, a proper public key set and the counter value is high enough
so that we can sign new eUICC packages correctly.
