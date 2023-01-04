(ns wator.core-spec
  (:require [speclj.core :refer :all]
            [wator
             [cell :as cell]
             [water :as water]
             [water-imp]
             [animal :as animal]
             [fish :as fish]
             [fish-imp]
             [world :as world]]
            [wator.config :as config]))

(describe "Wator"
  (with-stubs)
  (context "Water"
    (it "usually remains water"
      (with-redefs [rand (stub :rand {:return 0.0})]
        (let [water (water/make)
              evolved (cell/tick water)]
          (should= ::water/water (::cell/type evolved)))))

    (it "occasionally evolves into a fish"
      (with-redefs [rand (stub :rand {:return 1.0})]
        (let [water (water/make)
              evolved (cell/tick water)]
          (should= ::fish/fish (::cell/type evolved))))))

  (context "world"
    (it "creates a world full of water cells"
      (let [world (world/make 2 2)
            cells (::world/cells world)
            positions (set (keys cells))]
        (should= #{[0 0] [0 1]
                   [1 0] [1 1]} positions)
        (should (every? #(= ::water/water (::cell/type %)) (vals cells)))))

    (it "makes neighbors"
      (let [world (world/make 5 5)]
        (should= [[0 0] [0 1] [0 2]
                  [1 0] [1 2]
                  [2 0] [2 1] [2 2]]
                 (world/neighbors world [1 1]))
        (should= [[4 4] [4 0] [4 1]
                  [0 4] [0 1]
                  [1 4] [1 0] [1 1]]
                 (world/neighbors world [0 0]))
        (should= [[3 3] [3 4] [3 0]
                  [4 3] [4 0]
                  [0 3] [0 4] [0 0]]
                 (world/neighbors world [4 4])))))

  (context "animal"
    (it "moves"
      (let [fish (fish/make)
            world (-> (world/make 3 3)
                      (world/set-cell [1 1] fish))
            [loc cell] (animal/move fish [1 1] world)]
        (should= cell fish)
        (should (#{[0 0] [0 1] [0 2]
                   [1 0] [1 2]
                   [2 0] [2 1] [2 2]}
                 loc))))

    (it "doesn't move if there are no spaces"
      (let [fish (fish/make)
            world (-> (world/make 1 1)
                      (world/set-cell [0 0] fish))
            [loc cell] (animal/move fish [0 0] world)]
        (should= cell fish)
        (should= [0 0] loc)))

    (it "reproduces"
      (let [fish (-> (fish/make) (animal/set-age config/fish-reproduction-age))
            world (-> (world/make 3 3)
                      (world/set-cell [1 1] fish))
            [loc1 cell1 loc2 cell2] (animal/reproduce fish [1 1] world)]
        (should= loc1 [1 1])
        (should (fish/is? cell1))
        (should= 0 (animal/age cell1))
        (should (#{[0 0] [0 1] [0 2]
                   [1 0] [1 2]
                   [2 0] [2 1] [2 2]}
                 loc2))
        (should (fish/is? cell2))
        (should= 0 (animal/age cell2))))

    (it "doesn't reproduce if there is no room"
      (let [fish (-> (fish/make) (animal/set-age config/fish-reproduction-age))
            world (-> (world/make 1 1)
                      (world/set-cell [0 0] fish))
            failed (animal/reproduce fish [0 0] world)]
        (should-be-nil failed)))

    (it "doesn't reproduce if too young"
          (let [fish (-> (fish/make)
                         (animal/set-age (dec config/fish-reproduction-age)))
                world (-> (world/make 3 3)
                          (world/set-cell [1 1] fish))
                failed (animal/reproduce fish [1 1] world)]
            (should-be-nil failed)))))


