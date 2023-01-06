(ns wator.core-spec
  (:require [speclj.core :refer :all]
            [wator
             [cell :as cell]
             [water :as water]
             [water-imp]
             [animal :as animal]
             [fish :as fish]
             [fish-imp]
             [world :as world]
             [world-imp]]
            [wator.config :as config]))

(describe "Wator"
  (with-stubs)
  (context "Water"
    (it "usually remains water"
      (with-redefs [rand (stub :rand {:return 0.0})]
        (let [water (water/make)
              world (world/make 1 1)
              [from to] (cell/tick water [0 0] world)]
          (should-be-nil from)
          (should (water/is? (get to [0 0])))
          )))

    (it "occasionally evolves into a fish"
      (with-redefs [rand (stub :rand {:return 1.0})]
        (let [water (water/make)
              world (world/make 1 1)
              [from to] (cell/tick water [0 0] world)]
          (should-be-nil from)
          (should (fish/is? (get to [0 0])))))))

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
                 (world/neighbors world [4 4]))))

    (it "moves a fish around each tick"
      (doseq [scenario [{:dimension [2 1] :starting [0 0] :ending [1 0]}
                        {:dimension [2 1] :starting [1 0] :ending [0 0]}
                        {:dimension [1 2] :starting [0 0] :ending [0 1]}
                        {:dimension [1 2] :starting [0 1] :ending [0 0]}]]
        (let [fish (fish/make)
              {:keys [dimension starting ending]} scenario
              [h w] dimension
              small-world (-> (world/make h w)
                              (world/set-cell starting fish)
                              (world/tick))
              vacated-cell (world/get-cell small-world starting)
              occupied-cell (world/get-cell small-world ending)]
          (should (water/is? vacated-cell))
          (should (fish/is? occupied-cell))
          (should= 1 (animal/age occupied-cell)))))

    (it "fills the world with reproducing fish"
      (loop [world (-> (world/make 10 10)
                       (world/set-cell [5 5] (fish/make)))
             n 100]
        (if (zero? n)
          (let [cells (-> world ::world/cells vals)
                fishies (filter fish/is? cells)
                fish-count (count fishies)]
            (should (< 50 fish-count)))
          (recur (world/tick world) (dec n)))))

    (it "move two fish who compete for the same spot"
      (let [fish (fish/make)
            competative-world (-> (world/make 3 1)
                                  (world/set-cell [0 0] fish)
                                  (world/set-cell [2 0] fish)
                                  (world/tick))
            start-00 (world/get-cell competative-world [0 0])
            start-20 (world/get-cell competative-world [2 0])
            end-10 (world/get-cell competative-world [1 0])]
        (should (fish/is? end-10))
        (should (or (fish/is? start-00)
                    (fish/is? start-20)))
        (should (or (water/is? start-00)
                    (water/is? start-20)))))
    )

  (context "animal"
    (it "moves"
      (let [fish (fish/make)
            world (-> (world/make 3 3)
                      (world/set-cell [1 1] fish))
            [from to] (animal/move fish [1 1] world)
            loc (first (keys to))]
        (should (water/is? (get from [1 1])))
        (should (fish/is? (get to loc)))
        (should (#{[0 0] [0 1] [0 2]
                   [1 0] [1 2]
                   [2 0] [2 1] [2 2]}
                 loc))))

    (it "doesn't move if there are no spaces"
      (let [fish (fish/make)
            world (-> (world/make 1 1)
                      (world/set-cell [0 0] fish))
            [from to] (animal/move fish [0 0] world)]
        (should (fish/is? (get to [0 0])))
        (should (nil? from))))

    (it "reproduces"
      (let [fish (-> (fish/make) (animal/set-age config/fish-reproduction-age))
            world (-> (world/make 3 3)
                      (world/set-cell [1 1] fish))
            [from to] (animal/reproduce fish [1 1] world)
            from-loc (-> from keys first)
            from-cell (-> from vals first)
            to-loc (-> to keys first)
            to-cell (-> to vals first)]
        (should= from-loc [1 1])
        (should (fish/is? from-cell))
        (should= 0 (animal/age from-cell))
        (should (#{[0 0] [0 1] [0 2]
                   [1 0] [1 2]
                   [2 0] [2 1] [2 2]}
                 to-loc))
        (should (fish/is? to-cell))
        (should= 0 (animal/age to-cell))))

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


