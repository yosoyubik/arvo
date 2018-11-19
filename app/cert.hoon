::  Sends Eyre a %cert task
::
::::  /hoon/cert/hoon
  ::
/+  der, primitive-rsa, *pkcs
=,  eyre
=*  rsa  primitive-rsa
|%
+$  move  [bone card]
+$  card
  $%  [%cert wire (unit [wain wain])]
  ==
--
|_  [bow=bowl:gall $~]
++  this  .
++  poke-noun
  |=  a=*
  ^-  (quip move _this)
  fake
::  +fake: install fake cert for testing
::
::   XX remove
::
++  fake
  ^-  (quip move _this)
  =/  key=wain
    :~  '-----BEGIN PRIVATE KEY-----'
        'MIICdgIBADANBgkqhkiG9w0BAQEFAASCAmAwggJcAgEAAoGBAKNwapOQ6rQJHetP'
        'HRlJBIh1OsOsUBiXb3rXXE3xpWAxAha0MH+UPRblOko+5T2JqIb+xKf9Vi3oTM3t'
        'KvffaOPtzKXZauscjq6NGzA3LgeiMy6q19pvkUUOlGYK6+Xfl+B7Xw6+hBMkQuGE'
        'nUS8nkpR5mK4ne7djIyfHFfMu4ptAgMBAAECgYA+s0PPtMq1osG9oi4xoxeAGikf'
        'JB3eMUptP+2DYW7mRibc+ueYKhB9lhcUoKhlQUhL8bUUFVZYakP8xD21thmQqnC4'
        'f63asad0ycteJMLb3r+z26LHuCyOdPg1pyLk3oQ32lVQHBCYathRMcVznxOG16VK'
        'I8BFfstJTaJu0lK/wQJBANYFGusBiZsJQ3utrQMVPpKmloO2++4q1v6ZR4puDQHx'
        'TjLjAIgrkYfwTJBLBRZxec0E7TmuVQ9uJ+wMu/+7zaUCQQDDf2xMnQqYknJoKGq+'
        'oAnyC66UqWC5xAnQS32mlnJ632JXA0pf9pb1SXAYExB1p9Dfqd3VAwQDwBsDDgP6'
        'HD8pAkEA0lscNQZC2TaGtKZk2hXkdcH1SKru/g3vWTkRHxfCAznJUaza1fx0wzdG'
        'GcES1Bdez0tbW4llI5By/skZc2eE3QJAFl6fOskBbGHde3Oce0F+wdZ6XIJhEgCP'
        'iukIcKZoZQzoiMJUoVRrA5gqnmaYDI5uRRl/y57zt6YksR3KcLUIuQJAd242M/WF'
        '6YAZat3q/wEeETeQq1wrooew+8lHl05/Nt0cCpV48RGEhJ83pzBm3mnwHf8lTBJH'
        'x6XroMXsmbnsEw=='
        '-----END PRIVATE KEY-----'
    ==
  =/  cert=wain
    :~  '-----BEGIN CERTIFICATE-----'
        'MIIF8jCCBNqgAwIBAgITAPrPc8Udwmv5dJ+hx2Uh+gZF1TANBgkqhkiG9w0BAQsF'
        'ADAiMSAwHgYDVQQDDBdGYWtlIExFIEludGVybWVkaWF0ZSBYMTAeFw0xODA3MDMx'
        'ODAyMTZaFw0xODEwMDExODAyMTZaMB8xHTAbBgNVBAMTFHpvZC5keW5kbnMudXJi'
        'aXQub3JnMIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEAisQPzzmGWNZS'
        'NNAwY59XrqK/bU0NKNZS2ETOiJeSpzPAHYl+c39V96/QUR0tra2zQI4QD6kpMYX/'
        '7R5nwuvsA4o7ypfYupNrlzLPThCKEHpZomDD0Bb3T8u7YGrMjEX5cOmZIU2T/iy4'
        'GK/wWuBIy2TEp/0J+RoSCIr8Df/A7GIM8bwnv23Vq0kE2xBqqaT5LjvuQoXfiLJ4'
        '2F33DDno9lVikKEyt55D/08rH41KpXvn3tWZ46tZK6Ds7Zr1hEV1LbDx1CXDzQ6g'
        'KObBe54DWDV3h7TJhr0BSW68dFJhro7Y60NszTcFqY1RC9F0ePtsnKGFzMOe/U+f'
        'PvsGe2oWvwIDAQABo4IDIjCCAx4wDgYDVR0PAQH/BAQDAgWgMB0GA1UdJQQWMBQG'
        'CCsGAQUFBwMBBggrBgEFBQcDAjAMBgNVHRMBAf8EAjAAMB0GA1UdDgQWBBTokXAU'
        'vPwcrbkLxcVBCNNQ588pfjAfBgNVHSMEGDAWgBTAzANGuVggzFxycPPhLssgpvVo'
        'OjB3BggrBgEFBQcBAQRrMGkwMgYIKwYBBQUHMAGGJmh0dHA6Ly9vY3NwLnN0Zy1p'
        'bnQteDEubGV0c2VuY3J5cHQub3JnMDMGCCsGAQUFBzAChidodHRwOi8vY2VydC5z'
        'dGctaW50LXgxLmxldHNlbmNyeXB0Lm9yZy8wHwYDVR0RBBgwFoIUem9kLmR5bmRu'
        'cy51cmJpdC5vcmcwgf4GA1UdIASB9jCB8zAIBgZngQwBAgEwgeYGCysGAQQBgt8T'
        'AQEBMIHWMCYGCCsGAQUFBwIBFhpodHRwOi8vY3BzLmxldHNlbmNyeXB0Lm9yZzCB'
        'qwYIKwYBBQUHAgIwgZ4MgZtUaGlzIENlcnRpZmljYXRlIG1heSBvbmx5IGJlIHJl'
        'bGllZCB1cG9uIGJ5IFJlbHlpbmcgUGFydGllcyBhbmQgb25seSBpbiBhY2NvcmRh'
        'bmNlIHdpdGggdGhlIENlcnRpZmljYXRlIFBvbGljeSBmb3VuZCBhdCBodHRwczov'
        'L2xldHNlbmNyeXB0Lm9yZy9yZXBvc2l0b3J5LzCCAQIGCisGAQQB1nkCBAIEgfME'
        'gfAA7gB1ALDMg+Wl+X1rr3wJzChJBIcqx+iLEyxjULfG/SbhbGx3AAABZGGGG6QA'
        'AAQDAEYwRAIgJHrIawVea5/++wteocdbt1QUBxysW7uJqYgvnOWOQMgCIGRlioyE'
        'vzunUm/HZre3fF2jBsJr45C1tz5FTe/cYQwmAHUA3Zk0/KXnJIDJVmh9gTSZCEmy'
        'Sfe1adjHvKs/XMHzbmQAAAFkYYYjLQAABAMARjBEAiAWovIKERYeNbJlAKvNorwn'
        'RnSFP0lJ9sguwcpbcsYJ1gIgRJxTolkMOr0Fwq62q4UYnpREY8zu4hiL90Mhntky'
        'EwYwDQYJKoZIhvcNAQELBQADggEBAMYxvA+p4Qj0U23AHAe61W3+M6T1M0BfrGE2'
        'jJCaq4c3d7b9NEN1qFJHl8t/+Z/7RHUIzbm4CIOZynSM8mBxg2NgXymvXQkRrrBo'
        'fhO9u8Yxizx4+KOtiigt9JBVlpyCm6I9uifM+7rZYh45s2IkfDBPKd+M1tfIUOne'
        'YgUt1YguEkM2xqRG16JyHA0Xwn6mn+4pWiTdfNzlqol6vyGT7WfIvmV7cdGoYKjB'
        'wOt/g1wWMTwhSWBCVqCyn+f2rl8u3wbXrIUeRng2ryNVXO03nukTp7OLN3HUO6PR'
        'hC4NdS4o2geBNZr8RJiORtCelDaJprY7lhh2MFzVpsodc2eB5sQ='
        '-----END CERTIFICATE-----'
    ==
  =/  k=key:rsa  (need (ring:de:pem:pkcs8 key))
  =/  k8=wain  (ring:en:pem:pkcs8 k)
  :-  [ost.bow %cert /install `[k8 cert]]~
    this
--
