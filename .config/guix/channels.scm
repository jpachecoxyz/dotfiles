(list (channel
        (name 'nonguix)
        (url "https://gitlab.com/nonguix/nonguix")
        (branch "master")
        (introduction
          (make-channel-introduction
            "897c1a470da759236cc11798f4e0a5f7d4d59fbc"
            (openpgp-fingerprint
              "2A39 3FFF 68F4 EF7A 3D29  12AF 6F51 20A0 22FB B2D5"))))
      (channel
        (name 'guix)
        (url "https://git.savannah.gnu.org/git/guix.git")
        (branch "master")
        (introduction
          (make-channel-introduction
            "9edb3f66fd807b096b48283debdcddccfea34bad"
            (openpgp-fingerprint
              "BBB0 2DDF 2CEA F6A8 0D1D  E643 A2A0 6DF2 A33A 54FA"))))

      (channel
        (name 'saayix)
        (branch "main")
        (url "https://codeberg.org/look/saayix")
        (introduction
          (make-channel-introduction
            "12540f593092e9a177eb8a974a57bb4892327752"
            (openpgp-fingerprint
              "3FFA 7335 973E 0A49 47FC  0A8C 38D5 96BE 07D3 34AB"))))
      )